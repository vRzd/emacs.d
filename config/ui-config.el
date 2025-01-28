;; ============================
;; USER INTERFACE CONFIGURATIONS
;; ============================

(setq backup-directory-alist `(("." . "~/.emacs.d/emacs_backups")))

(global-set-key (kbd "C-c r") (lambda () (interactive) (load-file user-init-file)))
(global-set-key (kbd "C-x C-b") 'buffer-menu)


;;(global-display-line-numbers-mode 1)
;;(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell nil)

;; Ensure windmove is available
(require 'windmove)

;; Set the keybindings for window movement
(global-set-key (kbd "M-s-<up>") 'windmove-up)
(global-set-key (kbd "M-s-<down>") 'windmove-down)
(global-set-key (kbd "M-s-<left>") 'windmove-left)
(global-set-key (kbd "M-s-<right>") 'windmove-right)

;(global-set-key (kbd "C-c") 'kill-ring-save)  ; Copy
;(global-set-key (kbd "C-x") 'kill-region)     ; Cut
;(global-set-key (kbd "C-v") 'yank)            ; Paste
;; ============================
;; THEMING
;; ============================
(unless (package-installed-p 'modus-themes)
  (package-refresh-contents)
  (package-install 'modus-themes))

(use-package modus-themes
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-variable-pitch-ui t
        modus-themes-mixed-fonts t
        modus-themes-hl-line '(underline intense accented))
  (load-theme 'modus-vivendi t))

;; Font configuration
(set-face-attribute 'default nil :height 137)

;; Install which-key if not already installed
(unless (package-installed-p 'which-key)
  (package-refresh-contents)
  (package-install 'which-key))

;; Load and enable which-key mode
(require 'which-key)
(which-key-mode)

;; Optional customizations
(which-key-setup-side-window-bottom)
(setq which-key-idle-delay 0.5)
