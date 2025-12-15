;;; init.el --- Your config, fixed -*- lexical-binding: t; -*-

;; -------------------------------------
;; Package System (package.el only)
;; -------------------------------------
(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; -------------------------------------
;; MINIBUFFER FIX (THIS IS THE IMPORTANT PART)
;; -------------------------------------

;; Restore classic behavior for C-r and TAB
(setq completion-styles '(basic partial-completion))
(setq completion-category-defaults nil)
(setq completion-category-overrides nil)

;; Limit minibuffer height
(setq resize-mini-windows t)
(setq max-mini-window-height 3)

;; -------------------------------------
;; Vertico (ONLY presentation, not logic)
;; -------------------------------------
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-count 3)
  (vertico-cycle t))

;; -------------------------------------
;; Load YOUR existing config files
;; -------------------------------------
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(load "ui-config.el")
(load "evil-mode-config.el")   ;; untouched
(load "dired-config.el")
(load "calendar-config.el")
(load "org-mode-config.el")
(load "vterm-config.el")

;; -------------------------------------
;; macOS clipboard
;; -------------------------------------
(when (eq system-type 'darwin)
  (setq select-enable-clipboard t
        select-enable-primary t
        save-interprogram-paste-before-kill t)

  (global-set-key (kbd "s-c") #'kill-ring-save)
  (global-set-key (kbd "s-x") #'kill-region)
  (global-set-key (kbd "s-v") #'yank))

;; -------------------------------------
;; History & sanity
;; -------------------------------------
(save-place-mode 1)
(savehist-mode 1)
(recentf-mode 1)

(setq ring-bell-function 'ignore
      use-short-answers t)


;; Text
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-v C-t") #'toggle-truncate-lines))

(provide 'init)
;;; init.el ends here
