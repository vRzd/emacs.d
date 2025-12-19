;;; vterm-config.el --- SAFE vterm configuration -*- lexical-binding: t; -*-

;;; Code:

(use-package vterm
  :ensure t
  :commands vterm
  :init
  (setq vterm-max-scrollback 100000)

  (when (eq system-type 'darwin)
    (setq vterm-shell "/bin/zsh"
          vterm-shell-args '("--login")))

  :bind
  (:map vterm-mode-map
        ;; Navigation
        ("M-<left>"  . vterm-send-M-b)
        ("M-<right>" . vterm-send-M-f)

        ;; Editing
        ("C-a" . vterm-send-C-a)
        ("C-e" . vterm-send-C-e)
        ("C-k" . vterm-send-C-k)
        ("C-u" . vterm-send-C-u)
        ("C-d" . vterm-send-C-d)

        ;; Interrupt (SAFE)
        ("C-g" . vterm-send-C-c)

        ;; Paste
        ("C-y" . vterm-yank)))

;; ------------------------------------------------------------
;; REMOVE LINE HIGHLIGHT COMPLETELY
;; ------------------------------------------------------------
(defun my/vterm-disable-hl ()
  (hl-line-mode -1)
  (setq-local cursor-line-highlight nil)
  (setq-local global-hl-line-mode nil))

(add-hook 'vterm-mode-hook #'my/vterm-disable-hl)

;; ------------------------------------------------------------
;; CURSOR SHAPE
;; ------------------------------------------------------------
(add-hook 'vterm-mode-hook
          (lambda ()
            (setq-local cursor-type 'box)))

(add-hook 'vterm-copy-mode-hook
          (lambda ()
            (setq-local cursor-type 'bar)))

;; ------------------------------------------------------------
;; EVIL — HARD OFF
;; ------------------------------------------------------------
(add-hook 'vterm-mode-hook
          (lambda ()
            (when (bound-and-true-p evil-local-mode)
              (evil-local-mode -1))))

(with-eval-after-load 'evil-collection
  (setq evil-collection-mode-list
        (remove 'vterm evil-collection-mode-list)))

;; ------------------------------------------------------------
;; SCROLL FIX
;; ------------------------------------------------------------
(add-hook 'vterm-mode-hook
          (lambda ()
            (setq-local scroll-margin 0
                        scroll-conservatively 101
                        mouse-wheel-progressive-speed nil)))

;; ------------------------------------------------------------
;; ANSI COLORS (SSH / ls)
;; ------------------------------------------------------------
(with-eval-after-load 'vterm
  (set-face-foreground 'vterm-color-blue  "#7aa2f7")
  (set-face-foreground 'vterm-color-cyan  "#56b6c2")
  (set-face-foreground 'vterm-color-green "#98c379")
  (set-face-foreground 'vterm-color-red   "#e06c75")
  (set-face-foreground 'vterm-color-white "#d8dee9"))

;; ------------------------------------------------------------
;; macOS ENV
;; ------------------------------------------------------------
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(provide 'vterm-config)
;;; vterm-config.el ends here
