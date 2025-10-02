; ============================
;; PACKAGE INITIALIZATION
;; ============================
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)  
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(package-initialize)

(server-start)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Platform-specific shell configuration
(when (eq system-type 'darwin)  ;; macOS
  (setq exec-path-from-shell-shell-name "/bin/zsh")
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

(when (eq system-type 'windows-nt)  ;; Windows
  (setq explicit-shell-file-name "powershell.exe"))

;; STRAIGHT.EL setup
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))

;; Icons and Modeline
(use-package all-the-icons)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Path to your emacs directory
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; Load configuration files
(load "ui-config.el")
(load "evil-mode-config.el")
(load "dired-config.el")
(load "calendar-config.el")
(load "org-mode-config.el")
(load "vterm-config.el")
;;(load "helm-config.el")

;; Vertico for command completion
(use-package vertico
  :config
  (vertico-mode)
  (setq vertico-count 3)
  (setq vertico-cycle t))





(add-hook 'org-mode-hook (lambda () (flyspell-mode 1)))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

(use-package flyspell
  :ensure t
  :hook ((org-mode . flyspell-mode)
         (text-mode . flyspell-mode))
  :config
  ;; Remap correction key to `z=` in Normal mode (similar to Vim)
  (define-key evil-normal-state-map (kbd "z=") 'flyspell-correct-wrapper))

(defun flyspell-correct-wrapper ()
  "Call `flyspell-correct-at-point` or `flyspell-correct-word-before-point`."
  (interactive)
  (if (evil-normal-state-p)
      (flyspell-correct-at-point)
    (flyspell-correct-word-before-point)))



;; vterm terminal settings

;;(add-hook 'vterm-mode-hook (lambda () (evil-emacs-state)))
(add-hook 'vterm-mode-hook (lambda () (evil-local-mode -1)))


;(with-eval-after-load 'evil
;  (add-hook 'vterm-mode-hook
;            (lambda ()
;              (evil-local-mode -1)
;              (evil-escape-mode -1)
;              (setq-local evil-collection-mode nil))))
(defun my/disable-evil-in-vterm ()
  (when (derived-mode-p 'vterm-mode)
    (evil-local-mode -1)
    (evil-escape-mode -1)
    (setq-local evil-collection-mode nil)))

(add-hook 'vterm-mode-hook #'my/disable-evil-in-vterm)


(defun my/disable-evil-in-vterm ()
   (lambda ()
     (when (derived-mode-p 'vterm-mode)
       (evil-local-mode -1)
       (evil-escape-mode -1)
       (setq-local evil-collection-mode nil))))

(add-hook 'vterm-mode-hook #'my/disable-evil-in-vterm)

(add-hook 'vterm-mode-hook
          (lambda ()
            (evil-local-set-key 'insert (kbd "<delete>") 'vterm-send-delete)
            (evil-local-set-key 'normal (kbd "<delete>") 'vterm-send-delete)))

(use-package vterm
  :ensure t
  :config
  (define-key vterm-mode-map (kbd "M-<left>") 'vterm-send-M-b)
  (define-key vterm-mode-map (kbd "M-<right>") 'vterm-send-M-f)
  (define-key vterm-mode-map (kbd "<delete>") 'vterm-send-delete)
  (define-key vterm-mode-map (kbd "<kp-delete>") 'vterm-send-delete)
  (define-key vterm-mode-map (kbd "S-<backspace>") 'vterm-send-delete)
;  (define-key vterm-mode-map (kbd "C-r") nil)
;  (define-key vterm-mode-map (kbd "C-s") nil)
  (when (eq system-type 'darwin)
    (setq explicit-shell-file-name "zsh"))  ;; macOS zsh
  (when (eq system-type 'windows-nt)
    (setq explicit-shell-file-name "powershell.exe"))) ;; Windows



(add-hook 'vterm-mode-hook
          (lambda ()
            (define-key vterm-mode-map (kbd "<mouse-4>") nil) ; Forward scroll up
            (define-key vterm-mode-map (kbd "<mouse-5>") nil))) ; Forward scroll down


(use-package vterm-toggle
  :bind (("C-`" . vterm-toggle)))

(defun my/vterm-set-cursor-for-mode ()
  "Use a thin cursor in vterm copy mode, block cursor otherwise."
  (setq cursor-type (if (bound-and-true-p vterm-copy-mode)
                        'bar   ;; thin vertical bar in copy mode
                      'box))) ;; block cursor in normal mode

(add-hook 'vterm-copy-mode-hook #'my/vterm-set-cursor-for-mode)
(add-hook 'vterm-mode-hook #'my/vterm-set-cursor-for-mode)


(setq-default cursor-in-non-selected-windows nil)

(defun my/vterm-set-cursor-for-mode ()
  (setq cursor-type (if (bound-and-true-p vterm-copy-mode) 'bar 'box))
  (set-cursor-color (if (bound-and-true-p vterm-copy-mode) "orange" "white")))


(defvar my/vterm-pwd-result nil
  "Holds the output of remote pwd command.")

(defun my/vterm-get-remote-pwd ()
  "Run `pwd` inside vterm and get its result."
  (interactive)
  (when (eq major-mode 'vterm-mode)
    (let ((vterm-buffer (current-buffer)))
      (setq my/vterm-pwd-result nil)
      (let ((proc (get-buffer-process vterm-buffer)))
        (with-current-buffer vterm-buffer
          (goto-char (point-max))
          (let ((inhibit-read-only t))
            (vterm-send-string "pwd && echo __ENDPWD__\n"))
          (accept-process-output proc 0.2))
        (sleep-for 0.2) ;; give it a bit of time
        ;; Grab the last pwd line from buffer
        (with-current-buffer vterm-buffer
          (save-excursion
            (goto-char (point-max))
            (re-search-backward "__ENDPWD__" nil t)
            (forward-line -1)
            (setq my/vterm-pwd-result (string-trim (thing-at-point 'line t)))))))))

(defun open-remote-file-from-vterm ()
  "Open a remote file from vterm using TRAMP, starting at the remote shell's pwd."
  (interactive)
  (let ((remote-host "root@172.20.133.145"))
    (my/vterm-get-remote-pwd)
    (if my/vterm-pwd-result
        (let ((initial-path (concat "/ssh:" remote-host ":" my/vterm-pwd-result "/")))
          (find-file (read-file-name "Remote file: " initial-path)))
      (message "Could not detect remote PWD."))))

(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-c o r") #'open-remote-file-from-vterm))

(setq vterm-max-scrollback 100000)  ;; or any large number you prefer

;; Miscellaneous configurations
(recentf-mode 1)
(setq history-length 25)
(setq savehist-mode 1)
(setq save-place-mode 1)
(setq ring-bell-function 'ignore)

;; Enable mouse wheel tilt scroll, and configure it for macOS specifically
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction (if (eq system-type 'darwin) t nil))
(when (eq system-type 'darwin)
  (setq mac-mouse-wheel-smooth-scroll t))

;; Dired configuration
(require 'dired)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(setq use-short-answers t)

;; Auto-save settings
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/auto-save-list/" t)))

;; Custom variables (org-mode example)
(custom-set-variables
 '(org-capture-templates
   '(("t" "air travel" entry
      (id "travel")
      (file "~/.emacs.d/templates/research-air-travel.tpl")))))

(custom-set-faces)

;; Key bindings for resizing windows horizontally
(defun my-resize-window-horizontally (size)
  "Resize the window horizontally by SIZE."
  (adjust-window-trailing-edge (selected-window) size t))

(global-set-key (kbd "C-c C-h C--") (lambda () (interactive) (my-resize-window-horizontally -5)))  ;; Shrink
(global-set-key (kbd "C-c C-h C-=") (lambda () (interactive) (my-resize-window-horizontally 5)))   ;; Expand


(global-set-key (kbd "C-c w")
                (lambda ()
                  (interactive)
                  (message (format-time-string "%U"))))


(setq treesit-extra-load-path '("~/.emacs.d/tree-sitter/"))

(use-package json-mode
  :ensure t
  :hook ((json-mode . json-ts-mode)
         (json-ts-mode . hs-minor-mode)))



(use-package persp-mode
  :ensure t
  :init
  (setq persp-autosave-mode t)        ;; Enable auto-saving perspectives
  :config
  (persp-mode 1))                     ;; Turn on persp-mode

(setq persp-save-dir "~/.emacs.d/persp-confs/") ;; Directory to store perspectives
(add-hook 'kill-emacs-hook #'persp-save-state-to-file)
(add-hook 'emacs-startup-hook #'persp-load-state-from-file)

;; 

(global-set-key (kbd "C-c C-v C-w") 'visual-line-mode)
(global-set-key (kbd "C-c C-v C-t") 'toggle-truncate-lines)


(define-key global-map (kbd "C-c C-l C-n") 'display-line-numbers-mode)

(setq ispell-personal-dictionary "~/.emacs.d/.aspell.en.pws")


(setq org-agenda-custom-commands
      '(("c" "Tags in Current File"
         tags "" ;; Search all tags
         ((org-agenda-files (list buffer-file-name))))))

(defalias 'list-buffers 'ibuffer)
;; Remap C-x C-b to use ibuffer instead of list-buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

(defun ibuffer-search-by-name ()
  "Search buffers by name in ibuffer."
  (interactive)
  (let ((search-term (read-string "Search buffer name: ")))
    (ibuffer-filter-by-name search-term)))

;; Bind `/` to search by name in ibuffer
(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "/") 'ibuffer-search-by-name)
  (define-key ibuffer-mode-map (kbd "\\") 'ibuffer-filter-disable)) ; or choose another key

;; For Dired mode
(setq insert-directory-program "gls")
(setq dired-use-ls-dired t)


;; Follow symlinks without prompting
(setq vc-follow-symlinks t)













;; YAML
;;; ──────────────────────────────────────────────────────────────────────
;;;  YAML  ⟶  Tree‑sitter + TAB folding   (with safe fallback)
;;; ──────────────────────────────────────────────────────────────────────

;;;; 1 ▸  Where grammars live  ───────────────────────────────────────────
(defvar my/ts-dir (expand-file-name "tree-sitter" user-emacs-directory))
(make-directory my/ts-dir t)                       ;; create once
(setq treesit-language-build-root (expand-file-name "src"  my/ts-dir)
      treesit-extra-load-path     (list my/ts-dir))

;;;; 2 ▸  Tell Emacs where to fetch the YAML grammar  ───────────────────
(add-to-list 'treesit-language-source-alist
             '(yaml "https://github.com/ikatyang/tree-sitter-yaml") t)

;;;; 3 ▸  Build the grammar once (quiet)  ───────────────────────────────
(unless (treesit-language-available-p 'yaml)
  (ignore-errors (treesit-install-language-grammar 'yaml))) ; fails silently if no compiler

;;;; 4 ▸  Packages  ─────────────────────────────────────────────────────
;; Built‑ins: don’t let straight try to download them
(use-package yaml-ts-mode  :straight nil :mode ("\\.ya?ml\\'" . yaml-ts-mode))

;; Grab ts‑fold from GitHub if this Emacs build didn’t ship it
(use-package ts-fold
  :if (not (locate-library "ts-fold"))
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :defer t)

;; Visual indent guides (external helper)
(use-package highlight-indent-guides
  :straight t
  :hook ((yaml-ts-mode yaml-mode) . highlight-indent-guides-mode)
  :custom (highlight-indent-guides-method 'character))

;;;; 5 ▸  Master hook: choose the best folding backend  ─────────────────
(defun my/yaml-setup ()
  (setq indent-tabs-mode nil tab-width 2)

  (cond
   ;; 5‑a  Tree‑sitter folding path
   ((and (treesit-language-available-p 'yaml)
         (locate-library "ts-fold"))
    (require 'ts-fold)
    (ts-fold-mode 1) (ts-fold-indicators-mode 1)
    (evil-local-set-key 'normal (kbd "<tab>")   #'ts-fold-toggle)
    (evil-local-set-key 'normal (kbd "<S-tab>") #'ts-fold-toggle-recursively))

   ;; 5‑b  Safe fallback: indent‑based HideShow
   (t
    (hs-minor-mode 1)
    (setq-local hs-block-start-regexp "^\\s-*\\(?:[^:\n]+:\\)\\s-*")
    (evil-local-set-key 'normal (kbd "<tab>")   #'hs-toggle-hiding)
    (evil-local-set-key 'normal (kbd "<S-tab>") #'hs-hide-all))))

(add-hook 'yaml-ts-mode-hook #'my/yaml-setup)
(add-hook 'yaml-mode-hook    #'my/yaml-setup)   ;; just in case

;;;; 6 ▸  Optional linting (needs `pip install yamllint`)  ──────────────
(add-hook 'yaml-ts-mode-hook #'flymake-mode)
(add-hook 'yaml-mode-hook    #'flymake-mode)

;;; ── End YAML block ────────────────────────────────────────────────────
