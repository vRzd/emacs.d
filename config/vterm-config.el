;;; vterm-config.el --- Vterm config -*- lexical-binding: t; -*-

;;; Code:

(use-package vterm
  :ensure t
  :commands vterm
  :init
  ;; Big scrollback
  (setq vterm-max-scrollback 100000)

  ;; Shell selection
  (cond
   ((eq system-type 'darwin)
    (setq vterm-shell "/bin/zsh")
    ;; Make it behave like your normal login shell (loads ~/.zprofile then ~/.zshrc)
    (setq vterm-shell-args '("--login")))
   ((eq system-type 'windows-nt)
    ;; Only if you actually use vterm on Windows (often it won't work without MSYS2/WSL)
    (setq vterm-shell "powershell.exe")))

  :bind
  (:map vterm-mode-map
        ("M-<left>"  . vterm-send-M-b)
        ("M-<right>" . vterm-send-M-f)
        ("<delete>"  . vterm-send-delete)
        ("<kp-delete>" . vterm-send-delete)
        ("S-<backspace>" . vterm-send-delete)
        ;; Optional: make C-g send SIGINT (like a terminal)
        ("C-g" . vterm-send-C-c)))

(defun my/vterm-open-new ()
  (interactive)
  (vterm))

(global-set-key (kbd "C-c C-v C-t") #'my/vterm-open-new)

;; Toggle terminal (only if you really want it)
(use-package vterm-toggle
  :ensure t
  :bind (("C-`" . vterm-toggle)))

;; If you use Evil, disable it in vterm safely
(defun my/disable-evil-in-vterm ()
  (when (featurep 'evil)
    (evil-local-mode -1))
  (when (bound-and-true-p evil-escape-mode)
    (evil-escape-mode -1)))

(add-hook 'vterm-mode-hook #'my/disable-evil-in-vterm)

(with-eval-after-load 'evil-collection
  (setq evil-collection-mode-list (remove 'vterm evil-collection-mode-list)))

;; Cursor type for copy-mode (avoid global set-cursor-color)
(defun my/vterm-set-cursor-for-mode ()
  (setq-local cursor-type (if (bound-and-true-p vterm-copy-mode) 'bar 'box)))

(add-hook 'vterm-copy-mode-hook #'my/vterm-set-cursor-for-mode)
(add-hook 'vterm-mode-hook #'my/vterm-set-cursor-for-mode)

;; If your mouse wheel feels weird in vterm, keep it normal
(defun my/vterm-normal-mouse-scroll ()
  (setq-local mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq-local mouse-wheel-progressive-speed nil)
  (setq-local mouse-wheel-follow-mouse t)
  (setq-local scroll-margin 0)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-step 1))

(add-hook 'vterm-mode-hook #'my/vterm-normal-mouse-scroll)

;; macOS env sync for GUI Emacs (good to keep)
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(provide 'vterm-config)
;;; vterm-config.el ends here
