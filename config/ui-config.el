;;; ui-config.el --- Soft Grey UI -*- lexical-binding: t; -*-
;;; Commentary:
;; Clean modern UI with grey background, no borders, flat modeline.
;; No theme package required. Safe face handling.

;;; Code:

;; ----------------------------------------------------------
;; BASIC UI
;; ----------------------------------------------------------

(setq backup-directory-alist '(("." . "~/.emacs.d/emacs_backups")))

(global-set-key (kbd "C-c r")
                (lambda () (interactive) (load-file user-init-file)))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell nil)

(require 'windmove)
(global-set-key (kbd "M-s-<up>")    #'windmove-up)
(global-set-key (kbd "M-s-<down>")  #'windmove-down)
(global-set-key (kbd "M-s-<left>")  #'windmove-left)
(global-set-key (kbd "M-s-<right>") #'windmove-right)

;; ----------------------------------------------------------
;; CUSTOM GREY BACKGROUND (GLOBAL)
;; ----------------------------------------------------------

(set-face-attribute 'default nil
                    :background "#25252b"   ;; change this for more/less grey
                    :foreground "#d8dee9"
                    :height 137)

;; Fringe must be set AFTER default
(set-face-attribute 'fringe nil :background "#25252b")

;; ----------------------------------------------------------
;; HL-LINE (SAFE — NO CRASH)
;; ----------------------------------------------------------

(require 'hl-line)
(global-hl-line-mode 1)

(with-eval-after-load 'hl-line
  (set-face-attribute 'hl-line nil
                      :background "#2c2c32"))

;; ----------------------------------------------------------
;; DOOM-MODELINE (FLAT, NO HIGHLIGHT)
;; ----------------------------------------------------------

(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-height 22
        doom-modeline-bar-width 0
        doom-modeline-hud nil)
  (doom-modeline-mode 1))

(with-eval-after-load 'doom-modeline
  ;; File name / path — neutral
  (set-face-attribute 'doom-modeline-buffer-file nil
                      :background nil :foreground "#c5c8c6")

  (set-face-attribute 'doom-modeline-buffer-path nil
                      :background nil :foreground "#9aa0a4")

  ;; Neutralize all icons & states
  (dolist (face '(doom-modeline-project-dir
                  doom-modeline-buffer-major-mode
                  doom-modeline-buffer-minor-mode
                  doom-modeline-info
                  doom-modeline-warning
                  doom-modeline-urgent))
    (set-face-attribute face nil :foreground "#9aa0a4"))

  ;; Flat modeline — NO active highlight
  (set-face-attribute 'mode-line nil
                      :background "#1f1f23"
                      :foreground "#d0d0d0"
                      :box nil)

  (set-face-attribute 'mode-line-inactive nil
                      :background "#18181b"
                      :foreground "#777777"
                      :box nil))

;; ----------------------------------------------------------
;; WHICH-KEY
;; ----------------------------------------------------------

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.4))

;; ----------------------------------------------------------
;; REMOVE ALL BORDERS (ABSOLUTE)
;; ----------------------------------------------------------

(setq window-divider-default-right-width 0)
(setq window-divider-default-places nil)
(window-divider-mode -1)

(unless standard-display-table
  (setq standard-display-table (make-display-table)))
(set-display-table-slot standard-display-table 'vertical-border nil)




;; ----------------------------------------------------------
;; TEXT SUGAR
;; ----------------------------------------------------------
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-v C-t") #'toggle-truncate-lines))



(provide 'ui-config)
;;; ui-config.el ends here
