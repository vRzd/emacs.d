;;; init.el --- Core bootstrap only -*- lexical-binding: t; -*-

;; ============================================================
;; Package system (package.el only)
;; ============================================================

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

;; ============================================================
;; Custom file (keep Custom OUT of init.el)
;; ============================================================

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; ============================================================
;; Minibuffer CORE behavior (logic, not UI)
;; ============================================================

(setq completion-styles '(basic partial-completion)
      completion-category-defaults nil
      completion-category-overrides nil
      resize-mini-windows t
      max-mini-window-height 3
      enable-recursive-minibuffers t)

;; ============================================================
;; Save minibuffer / search history (REQUIRED for C-r)
;; ============================================================

(use-package savehist
  :init
  (setq history-length 1000
        savehist-additional-variables
        '(kill-ring search-ring regexp-search-ring))
  (savehist-mode 1))

;; ============================================================
;; Vertico (UI only, does NOT change logic)
;; ============================================================

(use-package vertico
  :init
  (vertico-mode 1)
  :custom
  (vertico-count 3)
  (vertico-cycle t))

;; ============================================================
;; Consult (history & navigation)
;; ============================================================

(use-package consult
  :config
  ;; Proper minibuffer completion integration
  (setq completion-in-region-function #'consult-completion-in-region)

  ;; Global, sane bindings (NOT minibuffer hacks)
  (global-set-key (kbd "C-s") #'consult-line)
  (global-set-key (kbd "M-y") #'consult-yank-pop))

;; ============================================================
;; Load modular configuration files
;; ============================================================

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; IMPORTANT:
;;  - Each file MUST end with (provide 'FEATURE)
;;  - NONE of these files may require themselves

(require 'ui-config)
(require 'evil-mode-config)
;;(require 'dired-config)
(require 'dirvish-config)
(require 'calendar-config)
(require 'org-config)
(require 'vterm-config)
(require 'clipboard-config)

;; ============================================================
;; General sanity
;; ============================================================

(save-place-mode 1)
(recentf-mode 1)

(setq ring-bell-function 'ignore
      use-short-answers t)

(provide 'init)
;;; init.el ends here
