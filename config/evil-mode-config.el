;;; evil-mode-config.el --- Evil only where explicitly enabled -*- lexical-binding: t; -*-

;; ============================================================
;; Evil core — STRICTLY LOCAL, NO UI POLLUTION
;; ============================================================

;; MUST be set before evil loads
(setq evil-want-keybinding nil)
(setq evil-want-minibuffer nil)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
        evil-want-fine-undo t
        evil-want-C-i-jump nil)

  :config
  ;; Start Evil engine (required for evil-local-mode)
  (evil-mode 1)

  ;; Default EVERYWHERE = Emacs state
  (setq evil-default-state 'emacs)

  ;; ----------------------------------------------------------
  ;; Enable Evil ONLY in real editing modes
  ;; ----------------------------------------------------------
  (dolist (hook '(org-mode-hook emacs-lisp-mode-hook))
    (add-hook hook
              (lambda ()
                (evil-local-mode 1)
                (evil-normal-state))))

  ;; ----------------------------------------------------------
  ;; NEVER allow Evil in minibuffer
  ;; ----------------------------------------------------------
  (add-hook 'minibuffer-setup-hook #'evil-emacs-state)

  ;; ----------------------------------------------------------
  ;; HARD exclusions (UI / control buffers)
  ;; ----------------------------------------------------------
  (dolist (mode '(help-mode
                  helpful-mode
                  Buffer-menu-mode
                  ibuffer-mode
                  Man-mode
                  woman-mode
                  vterm-mode
                  term-mode
                  eshell-mode
                  shell-mode
                  compilation-mode
                  messages-buffer-mode
                  special-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  ;; ----------------------------------------------------------
  ;; ESC always goes to Normal (only where Evil exists)
  ;; ----------------------------------------------------------
  (define-key evil-insert-state-map  [escape] #'evil-force-normal-state)
  (define-key evil-visual-state-map  [escape] #'evil-force-normal-state)
  (define-key evil-replace-state-map [escape] #'evil-force-normal-state))

;; ============================================================
;; Russian keyboard → Vim keys (ONLY in Evil buffers)
;; ============================================================

(use-package reverse-im
  :ensure t
  :demand t
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  ;; Enable ONLY when evil-local-mode is active
  (add-hook 'evil-local-mode-hook #'reverse-im-mode))

;; ============================================================
;; Evil Collection — ONLY Org (nothing else)
;; ============================================================

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(org))
  (evil-collection-init))

;; ============================================================
;; Undo Tree — quiet, no minibuffer spam
;; ============================================================

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-visualizer-diff nil
        undo-tree-visualizer-timestamps nil)
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

;; ============================================================
;; Minibuffer sanity (NO noise)
;; ============================================================

(add-hook 'minibuffer-setup-hook
          (lambda ()
            (eldoc-mode -1)))

(provide 'evil-mode-config)
;;; evil-mode-config.el ends her
