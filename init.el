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
(add-to-list 'load-path "~/.emacs.d/config/")

;; Load configuration files
(load "ui-config.el")
(load "evil-mode-config.el")
(load "dired-config.el")
(load "calendar-config.el")

;; Vertico for command completion
(use-package vertico
  :config
  (vertico-mode))

;; vterm terminal settings
(use-package vterm
  :ensure t
  :config
  (define-key vterm-mode-map (kbd "M-<left>") 'vterm-send-M-b)
  (define-key vterm-mode-map (kbd "M-<right>") 'vterm-send-M-f)
  (when (eq system-type 'darwin)
    (setq explicit-shell-file-name "zsh"))  ;; macOS zsh
  (when (eq system-type 'windows-nt)
    (setq explicit-shell-file-name "powershell.exe")))  ;; Windows PowerShell

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

