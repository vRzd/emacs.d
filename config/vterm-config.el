;;; vterm-config.el --- My Emacs config -*- lexical-binding: t; -*-

;; ============================
;; VTERM CONFIGURATION
;; ============================
(use-package vterm
  :ensure t
  :config
  ;; ----------------------------------
  ;; Shell setup (macOS / zsh)
  ;; ----------------------------------
  (when (eq system-type 'darwin)
    ;; Use zsh explicitly
    (setq vterm-shell "/bin/zsh")
    (setq explicit-shell-file-name "/bin/zsh")
    ;; Force login + interactive shell so that ~/.zprofile and ~/.zshrc load
    (setq explicit-zsh-args '("--login" "--interactive")))

  (when (eq system-type 'windows-nt)
    (setq explicit-shell-file-name "powershell.exe"))

  ;; ----------------------------------
  ;; Keybindings
  ;; ----------------------------------
  
  (define-key vterm-mode-map (kbd "M-<left>")  #'vterm-send-M-b)
  (define-key vterm-mode-map (kbd "M-<right>") #'vterm-send-M-f)
  (define-key vterm-mode-map (kbd "<delete>")  #'vterm-send-delete)
  (define-key vterm-mode-map (kbd "<kp-delete>") #'vterm-send-delete)
  (define-key vterm-mode-map (kbd "S-<backspace>") #'vterm-send-delete)

  ;; ----------------------------------
  ;; Mouse scroll passthrough
  ;; ----------------------------------
  (define-key vterm-mode-map [mouse-4] nil)
  (define-key vterm-mode-map [mouse-5] nil)

  ;; ----------------------------------
  ;; Scrollback
  ;; ----------------------------------
  (setq vterm-max-scrollback 100000))

;; ----------------------------------
;; Toggle terminal
;; ----------------------------------
(use-package vterm-toggle
  :bind (("C-`" . vterm-toggle)))

;; ----------------------------------
;; Disable evil mode in vterm
;; ----------------------------------
(defun my/disable-evil-in-vterm ()
  (evil-local-mode -1)
  (evil-escape-mode -1)
  (setq-local evil-collection-mode nil))

(add-hook 'vterm-mode-hook #'my/disable-evil-in-vterm)

(with-eval-after-load 'evil-collection
  (setq evil-collection-mode-list (remove 'vterm evil-collection-mode-list)))

;; ----------------------------------
;; Cursor style for copy mode
;; ----------------------------------
(defun my/vterm-set-cursor-for-mode ()
  (setq cursor-type (if (bound-and-true-p vterm-copy-mode) 'bar 'box))
  (set-cursor-color (if (bound-and-true-p vterm-copy-mode) "orange" "white")))

(add-hook 'vterm-copy-mode-hook #'my/vterm-set-cursor-for-mode)
(add-hook 'vterm-mode-hook #'my/vterm-set-cursor-for-mode)

;; ----------------------------------
;; Disable mouse scroll hijack
;; ----------------------------------
(defun my/vterm-disable-mouse-scroll ()
  (setq-local mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq-local mouse-wheel-progressive-speed nil)
  (setq-local mouse-wheel-follow-mouse 't)
  (setq-local scroll-margin 0)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-step 1))

(add-hook 'vterm-mode-hook #'my/vterm-disable-mouse-scroll)

;; ----------------------------------
;; Ensure macOS env variables sync with shell
;; ----------------------------------
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(defun my/vterm-open-new ()
  (interactive)
  (vterm))

(global-set-key (kbd "C-c C-v C-t") #'my/vterm-open-new)

(provide 'vterm-config)
;;; vterm-config.el ends here

