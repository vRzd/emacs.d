;; ============================
;; PACKAGE INITIALIZATION
;; ============================
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)  
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(package-initialize)

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

(add-hook 'vterm-mode-hook (lambda () (evil-emacs-state)))
(add-hook 'vterm-mode-hook (lambda () (evil-local-mode -1)))

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
  (when (eq system-type 'darwin)
    (setq explicit-shell-file-name "zsh"))  ;; macOS zsh
  (when (eq system-type 'windows-nt)
    (setq explicit-shell-file-name "powershell.exe"))) ;; Windows

(add-hook 'vterm-mode-hook
          (lambda ()
            (define-key vterm-mode-map (kbd "<mouse-4>") nil) ; Forward scroll up
            (define-key vterm-mode-map (kbd "<mouse-5>") nil))) ; Forward scroll down




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

