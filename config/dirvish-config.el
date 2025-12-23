;;; dirvish-config.el --- Modern Dirvish (Emacs-native) -*- lexical-binding: t; -*-

;;; Commentary:
;; Dirvish configured as a *lightweight Dired enhancement*
;; NOT a standalone file manager.
;;
;; Principles:
;; - Dired semantics stay intact
;; - Preview is transient, edit is explicit
;; - No Evil inside Dirvish
;; - No C-c pollution
;; - Load-order safe (Emacs 31)

;;; Code:

(require 'dired)
(require 'dirvish)

;; ------------------------------------------------------------
;; ENABLE DIRVISH (SAFE OVERRIDE)
;; ------------------------------------------------------------

(dirvish-override-dired-mode)

;; ------------------------------------------------------------
;; CORE BEHAVIOR — BEST PRACTICE
;; ------------------------------------------------------------

(setq dirvish-reuse-session t)          ;; reuse same buffer
(setq dirvish-use-header-line t)        ;; show path + metadata
(setq dirvish-use-mode-line nil)        ;; clean UI
(setq dirvish-preview-max-size (* 10 1024 1024)) ;; 10MB limit
(setq dirvish-preview-dispatchers
      '(image video audio gif pdf archive))

(add-hook 'dirvish-mode-hook #'auto-revert-mode)

;; ------------------------------------------------------------
;; ABSOLUTELY DISABLE EVIL IN DIRVISH
;; ------------------------------------------------------------

(defun my/dirvish-disable-evil ()
  "Disable Evil locally in Dirvish buffers."
  (when (bound-and-true-p evil-local-mode)
    (evil-local-mode -1))
  (when (bound-and-true-p evil-collection-mode)
    (evil-collection-mode -1)))

(add-hook 'dirvish-mode-hook #'my/dirvish-disable-evil)

;; ------------------------------------------------------------
;; KEYBINDINGS — STANDARD DIRED SEMANTICS
;; ------------------------------------------------------------

(with-eval-after-load 'dirvish
  ;; Open / enter
  (define-key dirvish-mode-map (kbd "RET") #'dired-find-file)

  ;; Go up
  (define-key dirvish-mode-map (kbd "^") #'dired-up-directory)
  (define-key dirvish-mode-map (kbd "<backspace>") #'dired-up-directory)

  ;; Preview (non-editing)
  (define-key dirvish-mode-map (kbd "SPC") #'dirvish-preview-toggle)

  ;; Quit window only
  (define-key dirvish-mode-map (kbd "q") #'quit-window)

  ;; Refresh
  (define-key dirvish-mode-map (kbd "g") #'revert-buffer))

;; ------------------------------------------------------------
;; DIRED QUALITY OF LIFE
;; ------------------------------------------------------------

(setq dired-listing-switches "-alh --group-directories-first")
(setq delete-by-moving-to-trash t)
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-isearch-filenames 'dwim) ;; ignore case, smart

;; ------------------------------------------------------------
;; HIDE . AND .. (LOAD-ORDER SAFE)
;; ------------------------------------------------------------

(with-eval-after-load 'dired-x
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\.\\.?$")))

(add-hook 'dirvish-mode-hook #'dired-omit-mode)

;; ------------------------------------------------------------
;; TOGGLE PARENT COLUMN (p)
;; ------------------------------------------------------------

(defun my/dirvish-toggle-parent ()
  "Toggle Dirvish parent directory column."
  (interactive)
  (if (memq 'parent dirvish-attributes)
      (setq dirvish-attributes (remove 'parent dirvish-attributes))
    (add-to-list 'dirvish-attributes 'parent))
  (dirvish-revert))

(with-eval-after-load 'dirvish
  (define-key dirvish-mode-map (kbd "p") #'my/dirvish-toggle-parent))

;; ------------------------------------------------------------
;; FIX CURSOR ARTIFACT (block cursor at column 1)
;; ------------------------------------------------------------

(defun my/dirvish-cursor-fix ()
  "Use thin cursor to avoid block artifact."
  (setq-local cursor-type 'bar)
  (setq-local cursor-in-non-selected-windows nil))

(add-hook 'dirvish-mode-hook #'my/dirvish-cursor-fix)

;; ------------------------------------------------------------
;; VTERM FROM DIRVISH (NO C-c ABUSE)
;; ------------------------------------------------------------

(defun my/dirvish-open-vterm ()
  "Open vterm in current Dirvish directory."
  (interactive)
  (let ((default-directory (dirvish-current-directory)))
    (vterm)))

(with-eval-after-load 'dirvish
  (define-key dirvish-mode-map (kbd "V") #'my/dirvish-open-vterm))

;; ------------------------------------------------------------
;; ENTRY POINT
;; ------------------------------------------------------------

(defun my/dirvish-home ()
  "Open Dirvish in home directory."
  (interactive)
  (dirvish "~"))

(global-set-key (kbd "C-x d") #'my/dirvish-home)

(provide 'dirvish-config)
;;; dirvish-config.el ends here
