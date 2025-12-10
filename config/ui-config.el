;; ============================
;; USER INTERFACE CONFIGURATIONS
;; ============================

(setq backup-directory-alist `(("." . "~/.emacs.d/emacs_backups")))

(global-set-key (kbd "C-c r") (lambda () (interactive) (load-file user-init-file)))
(global-set-key (kbd "C-x C-b") 'buffer-menu)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell nil)

;; Ensure windmove is available
(require 'windmove)
(global-set-key (kbd "M-s-<up>") 'windmove-up)
(global-set-key (kbd "M-s-<down>") 'windmove-down)
(global-set-key (kbd "M-s-<left>") 'windmove-left)
(global-set-key (kbd "M-s-<right>") 'windmove-right)

;; ============================
;; THEMING
;; ============================

(unless (package-installed-p 'modus-themes)
  (package-refresh-contents)
  (package-install 'modus-themes))

(use-package modus-themes
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-variable-pitch-ui t
        modus-themes-mixed-fonts t
        modus-themes-hl-line '(underline intense accented))
  (load-theme 'modus-vivendi t))

;; Font height (keep your font choice)
(set-face-attribute 'default nil :height 137)

;; Doom modeline (optional)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 20)))

;; Save cursor location
(save-place-mode 1)

;; Which-key popup
(unless (package-installed-p 'which-key)
  (package-refresh-contents)
  (package-install 'which-key))

(require 'which-key)
(which-key-mode)
(which-key-setup-side-window-bottom)
(setq which-key-idle-delay 0.5)

;; ============================
;; FACE OVERRIDES
;; ============================

;; Default background/foreground override
(set-face-attribute 'default nil
                    :background "#1e2430"
                    :foreground "#d0d0d0")

(custom-set-faces
 ;; Modeline
 '(mode-line ((t (:background "#2a2f40" :foreground "#ffffff"))))
 '(mode-line-inactive ((t (:background "#1e2430" :foreground "#888888"))))
 ;; Prompt and syntax
 '(minibuffer-prompt ((t (:foreground "#7aa2f7" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "#7aa2f7"))))
 '(font-lock-builtin-face ((t (:foreground "#7e956e"))))
 ;; Divider
 '(window-divider ((t (:background "#ffffff"))))
 '(window-divider-first-pixel ((t (:background "#ffffff"))))
 '(window-divider-last-pixel ((t (:background "#ffffff"))))
 '(vertical-border ((t (:foreground "#ffffff" :background "#ffffff"))))
 ;; Fringe (removes black space)
 '(fringe ((t (:background "#1e2430"))))
 ;; Region selection
 '(region ((t (:background "#44475a"))))
 ;; Cursor line highlight (optional)
 '(hl-line ((t (:background "#262a3f"))))
 ;; Vterm
 '(vterm-color-blue ((t (:foreground "#5d7f71")))))

;; ============================
;; WINDOW DIVIDER
;; ============================

(setq window-divider-default-right-width 1)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

;; Use glyph for vertical border (optional: fancy line)
(unless standard-display-table
  (setq standard-display-table (make-display-table)))
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?│))  ;; thin bar

;; ============================
;; VTERM CONFIGURATION
;; ============================

(defun my/vterm-clean-ls-colors ()
  "Set a cleaner LS_COLORS to remove green backgrounds in vterm."
  (when (string= major-mode "vterm-mode")
    (vterm-send-string "export LS_COLORS='di=1;34:fi=0:ln=1;36:ex=1;32'\n")))

(add-hook 'vterm-mode-hook #'my/vterm-clean-ls-colors)
