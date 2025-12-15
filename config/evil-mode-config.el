;;; evil-mode-config.el --- Evil only where explicitly enabled -*- lexical-binding: t; -*-

;; ============================================================
;; Evil core (local-only, clean, reload-safe)
;; ============================================================

;; IMPORTANT: must be set BEFORE evil loads
(setq evil-want-keybinding nil)
(setq evil-want-minibuffer nil) ;; <- critical: prevents Evil from touching minibuffer

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
        evil-want-fine-undo t
        evil-want-C-i-jump nil)

  :config
  ;; Start Evil engine (needed so evil-local-mode works)
  (evil-mode 1)

  ;; Default everywhere = Emacs state (so nothing gets Evil unless we enable it)
  (setq evil-default-state 'emacs)

  ;; Enable Evil ONLY in Org & Emacs Lisp
  (dolist (hook '(org-mode-hook emacs-lisp-mode-hook))
    (add-hook hook
              (lambda ()
                (evil-local-mode 1)
                (evil-normal-state))))

  ;; Extra safety: minibuffer always Emacs state
  (add-hook 'minibuffer-setup-hook #'evil-emacs-state)

  ;; Hard exclusions (safety net)
  (dolist (mode '(help-mode helpful-mode Man-mode woman-mode
                  vterm-mode term-mode eshell-mode shell-mode
                  compilation-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  ;; ESC always goes to Normal (only matters in buffers where Evil is active)
  (define-key evil-insert-state-map  [escape] #'evil-force-normal-state)
  (define-key evil-visual-state-map  [escape] #'evil-force-normal-state)
  (define-key evil-replace-state-map [escape] #'evil-force-normal-state))

;; ============================================================
;; Russian keyboard → Vim keys
;; ============================================================

(use-package reverse-im
  :ensure t
  :demand t
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode 1))

;; ============================================================
;; Evil Collection — ONLY Org
;; ============================================================

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(org))
  (evil-collection-init))

;; ============================================================
;; Undo Tree
;; ============================================================

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  (evil-set-undo-system 'undo-tree))

;; ============================================================
;; Org-specific Evil keys
;; ============================================================

(with-eval-after-load 'org
  (with-eval-after-load 'evil
    (evil-define-key 'normal org-mode-map
      (kbd "RET") #'org-open-at-point
      (kbd "TAB") #'org-cycle)))

(provide 'evil-mode-config)
;;; evil-mode-config.el ends here