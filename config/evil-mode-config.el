;;; evil-config.el --- Evil setup for Org & Elisp only

;; ============================
;; Base Evil Setup
;; ============================
(setq evil-want-keybinding nil) ;; must be set before loading evil

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-fine-undo t)
  (setq evil-want-C-i-jump nil)
  :config
  ;; DO NOT enable globally → no (evil-mode 1)

  ;; Enable Evil ONLY in Org and Emacs Lisp buffers
  (add-hook 'org-mode-hook #'evil-local-mode)
  (add-hook 'emacs-lisp-mode-hook #'evil-local-mode)

  ;; Cursor appearance
  (setq evil-normal-state-cursor '("white" box))
  (setq evil-insert-state-cursor '("green" bar))
  (setq evil-visual-state-cursor '("orange" bar))

  ;; Fix when switching keyboard layouts (Russian <-> English)
  (add-hook 'input-method-activate-hook  #'evil-normalize-keymaps)
  (add-hook 'input-method-inactivate-hook #'evil-normalize-keymaps))

;; ============================
;; Evil Collection (for org + elisp + help)
;; ============================
(use-package evil-collection
  :after evil
  :config
  ;; DO NOT load for org-agenda
  (setq evil-collection-mode-list '(org help emacs-lisp))
  (evil-collection-init))

;; ============================
;; Undo Tree
;; ============================
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree))

(unless (file-exists-p "~/.emacs.d/undo")
  (make-directory "~/.emacs.d/undo"))

;; ============================
;; Remove conflicting bindings
;; ============================
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-_") nil)
  (define-key evil-visual-state-map (kbd "C-_") nil)
  (define-key evil-insert-state-map (kbd "C-_") nil)
  (define-key evil-motion-state-map (kbd "C-_") nil))

;; ============================
;; macOS Clipboard Sync
;; ============================
(setq select-enable-clipboard t)
(setq save-interprogram-paste-before-kill t)

(defun my/copy-to-macos-clipboard (text &optional _push)
  "Copy TEXT to macOS clipboard using pbcopy."
  (when (eq system-type 'darwin)
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max) "pbcopy"))))

(defun my/paste-from-macos-clipboard ()
  "Paste from macOS clipboard."
  (when (eq system-type 'darwin)
    (shell-command-to-string "pbpaste")))

(setq interprogram-cut-function #'my/copy-to-macos-clipboard)
(setq interprogram-paste-function #'my/paste-from-macos-clipboard)

;; Visual-mode yank → macOS clipboard
(defun my/evil-yank-macos (beg end &optional _type _reg _handler)
  (interactive "r")
  (evil-yank beg end _type _reg _handler)
  (let ((text (buffer-substring-no-properties beg end)))
    (my/copy-to-macos-clipboard text)))

(with-eval-after-load 'evil
  (evil-define-key 'visual org-mode-map         (kbd "y") #'my/evil-yank-macos)
  (evil-define-key 'visual emacs-lisp-mode-map  (kbd "y") #'my/evil-yank-macos))

;; ============================
;; Org-specific keys (ONLY org-mode)
;; ============================
(with-eval-after-load 'org
  (evil-define-key 'normal org-mode-map
    (kbd "RET") #'org-open-at-point
    (kbd "TAB") #'org-cycle)) ;; cycle only inside org-buffer

(provide 'evil-config)
;;; evil-config.el ends here
