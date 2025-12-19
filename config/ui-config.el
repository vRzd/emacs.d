;;; ui-config.el --- Soft Grey UI -*- lexical-binding: t; -*-
;;; Commentary:
;; Clean modern UI with grey background, no borders, flat modeline.
;; Includes safe vterm color overrides for readable SSH / ls output.

;;; Code:

;; ----------------------------------------------------------
;; BASIC UI
;; ----------------------------------------------------------

(setq backup-directory-alist '(("." . "~/.emacs.d/emacs_backups")))

(global-set-key (kbd "C-c r")
                (lambda () (interactive) (load-file user-init-file)))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq visible-bell nil)

;; ----------------------------------------------------------
;; WINDOW NAVIGATION
;; ----------------------------------------------------------

(require 'windmove)
(global-set-key (kbd "M-s-<up>")    #'windmove-up)
(global-set-key (kbd "M-s-<down>")  #'windmove-down)
(global-set-key (kbd "M-s-<left>")  #'windmove-left)
(global-set-key (kbd "M-s-<right>") #'windmove-right)

;; ----------------------------------------------------------
;; BASE COLORS (GLOBAL)
;; ----------------------------------------------------------

(set-face-attribute 'default nil
                    :background "#25252b"
                    :foreground "#d8dee9"
                    :height 137)

;; Fringe must follow default
(set-face-attribute 'fringe nil
                    :background "#25252b")

;; ----------------------------------------------------------
;; HL-LINE (SAFE)
;; ----------------------------------------------------------

(require 'hl-line)
(global-hl-line-mode 1)

(with-eval-after-load 'hl-line
  (set-face-attribute 'hl-line nil
                      :background "#2c2c32"))

;; ----------------------------------------------------------
;; MODELINE (DOOM-MODELINE, FLAT)
;; ----------------------------------------------------------

(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-height 22
        doom-modeline-bar-width 0
        doom-modeline-hud nil
        doom-modeline-icon nil)
  (doom-modeline-mode 1))

(with-eval-after-load 'doom-modeline
  ;; Active modeline
  (set-face-attribute 'mode-line nil
                      :background "#1f1f23"
                      :foreground "#d0d0d0"
                      :box nil)

  ;; Inactive modeline
  (set-face-attribute 'mode-line-inactive nil
                      :background "#18181b"
                      :foreground "#777777"
                      :box nil)

  ;; Neutralize noisy faces
  (dolist (face '(doom-modeline-project-dir
                  doom-modeline-buffer-file
                  doom-modeline-buffer-path
                  doom-modeline-buffer-major-mode
                  doom-modeline-buffer-minor-mode
                  doom-modeline-info
                  doom-modeline-warning
                  doom-modeline-urgent))
    (set-face-attribute face nil
                        :background nil
                        :foreground "#9aa0a4")))

;; ----------------------------------------------------------
;; WHICH-KEY
;; ----------------------------------------------------------

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.4)
  (which-key-mode 1))

;; ----------------------------------------------------------
;; REMOVE ALL WINDOW BORDERS
;; ----------------------------------------------------------

(setq window-divider-default-right-width 0
      window-divider-default-places nil)

(window-divider-mode -1)

(unless standard-display-table
  (setq standard-display-table (make-display-table)))

(set-display-table-slot standard-display-table 'vertical-border nil)

;; ----------------------------------------------------------
;; VTERM COLOR FIX (SSH / ls readability)
;; ----------------------------------------------------------
;; vterm uses ANSI faces, NOT font-lock or default faces.
;; These overrides keep remote shells readable.

(with-eval-after-load 'vterm
  ;; Directories
  (set-face-foreground 'vterm-color-blue  "#7aa2f7")
  (set-face-bold       'vterm-color-blue  t)

  ;; Symlinks / special
  (set-face-foreground 'vterm-color-cyan  "#56b6c2")

  ;; Executables
  (set-face-foreground 'vterm-color-green "#98c379")

  ;; Archives / warnings
  (set-face-foreground 'vterm-color-red   "#e06c75")

  ;; Numbers / subtle text
  (set-face-foreground 'vterm-color-white "#d8dee9"))

;; ----------------------------------------------------------
;; ORG SMALL EXTRAS (UI ONLY)
;; ----------------------------------------------------------

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-t") #'toggle-truncate-lines))

;; ----------------------------------------------------------
;; HORIZONTAL SCROLLING (REAL, PREDICTABLE)
;; ----------------------------------------------------------

(setq-default truncate-lines t)
(setq-default hscroll-margin 2)
(setq-default hscroll-step 1)

(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

(global-set-key (kbd "C-S-<left>")  #'scroll-right)
(global-set-key (kbd "C-S-<right>") #'scroll-left)
;;; MOUSE
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)


(provide 'ui-config)
;;; ui-config.el ends here
