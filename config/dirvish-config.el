;;; dirvish-config.el --- Dired + TRUE manual preview -*- lexical-binding: t; -*-

(require 'dired)
(require 'dired-x)

;; ------------------------------------------------------------
;; DIRED BASE
;; ------------------------------------------------------------

(setq dired-listing-switches "-alh --group-directories-first")
(setq dired-isearch-filenames 'dwim)
(setq delete-by-moving-to-trash t)

(setq dired-omit-files
      (concat dired-omit-files "\\|^\\.\\.?$"))
(add-hook 'dired-mode-hook #'dired-omit-mode)

(define-key dired-mode-map (kbd "<backspace>") #'dired-up-directory)
(define-key dired-mode-map (kbd "DEL") #'dired-up-directory)

;; ------------------------------------------------------------
;; SORT PROMPT
;; ------------------------------------------------------------

(defun my/dired-sort-prompt ()
  (interactive)
  (pcase
      (read-char-choice
       "Sort by: [n]ame [e]xt [d]ate [s]ize: "
       '(?n ?e ?d ?s))
    (?n (dired-sort-other "-alh --group-directories-first"))
    (?e (dired-sort-other "-alh --group-directories-first -X"))
    (?d (dired-sort-other "-alh --group-directories-first -t"))
    (?s (dired-sort-other "-alh --group-directories-first -S"))))

(define-key dired-mode-map (kbd "s") #'my/dired-sort-prompt)

;; ------------------------------------------------------------
;; MANUAL PREVIEW (NO DIRVISH OVERRIDE)
;; ------------------------------------------------------------

(when (require 'dirvish nil t)

  ;; Preview-safe types only
  (setq dirvish-preview-dispatchers '(image gif pdf archive))
  (setq dirvish-preview-max-size (* 10 1024 1024))

  ;; REAL manual preview command
  (defun my/dired-preview ()
    "Preview file at point in another window without auto preview."
    (interactive)
    (let ((file (dired-get-file-for-visit)))
      (display-buffer
       (find-file-noselect file)
       '((display-buffer-reuse-window
          display-buffer-in-side-window)
         (side . right)
         (window-width . 0.45)))))

  ;; SPACE = preview
  (define-key dired-mode-map (kbd "SPC") #'my/dired-preview))

;; ------------------------------------------------------------
;; ENTRY
;; ------------------------------------------------------------

(defun my/dired-home ()
  (interactive)
  (dired "~"))

(global-set-key (kbd "C-x d") #'my/dired-home)

(define-key dired-mode-map (kbd "/") #'dired-goto-file)

(setq dired-clean-confirm-killing-deleted-buffers nil)

(provide 'dirvish-config)
;;; dirvish-config.el ends here
