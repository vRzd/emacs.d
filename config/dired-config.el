;;; dired-config.el --- Clean, modern Dired (Emacs-native) -*- lexical-binding: t; -*-

;;; Commentary:
;; Stable, Evil-free Dired configuration.
;; Optimized for macOS and keyboard-driven navigation.

;;; Code:

(require 'dired)

;; ============================================================
;; Core Dired setup
;; ============================================================

(use-package dired
  :ensure nil                     ;; built-in
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom
  ;; Prefer GNU ls on macOS (for colors, grouping)
  (insert-directory-program
   (or (executable-find "gls") insert-directory-program))

  ;; Human-readable, directories first
  (dired-listing-switches "-alh --group-directories-first")

  ;; Move files to trash instead of deleting
  (delete-by-moving-to-trash t)

  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode)))

;; ============================================================
;; Quality-of-life navigation
;; ============================================================

(with-eval-after-load 'dired
  ;; Reuse buffer when entering directories
  (put 'dired-find-alternate-file 'disabled nil)

  (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^")   #'dired-up-directory)
  (define-key dired-mode-map (kbd "b")   #'dired-jump))

;; ============================================================
;; Dotfiles toggle
;; ============================================================

(use-package dired-hide-dotfiles
  :ensure t
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
              ("H" . dired-hide-dotfiles-mode)))

;; ============================================================
;; Icons (optional but nice)
;; ============================================================

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

;; ============================================================
;; Open files with macOS default apps
;; ============================================================

(use-package dired-open
  :ensure t
  :custom
  (dired-open-extensions
   '(("png" . "open")
     ("jpg" . "open")
     ("jpeg" . "open")
     ("pdf" . "open")
     ("mp4" . "open")
     ("mov" . "open"))))

;; ============================================================
;; Create files & directories
;; ============================================================

(defun my/dired-create-file ()
  "Create an empty file in the current Dired directory."
  (interactive)
  (let* ((dir (dired-current-directory))
         (name (read-string "New file: "))
         (path (expand-file-name name dir)))
    (if (file-exists-p path)
        (user-error "File already exists: %s" name)
      (write-region "" nil path)
      (revert-buffer)
      (message "Created file: %s" name))))

(defun my/dired-create-directory ()
  "Create a directory in the current Dired directory."
  (interactive)
  (let* ((dir (dired-current-directory))
         (name (read-string "New directory: "))
         (path (expand-file-name name dir)))
    (if (file-exists-p path)
        (user-error "Directory already exists: %s" name)
      (make-directory path t)
      (revert-buffer)
      (message "Created directory: %s" name))))

;; ============================================================
;; Search / jump inside Dired
;; ============================================================

(defun my/dired-search ()
  "Search for a filename substring in the current Dired buffer."
  (interactive)
  (call-interactively #'dired-isearch-filenames))

;; ============================================================
;; Custom keybindings (Emacs-native)
;; ============================================================

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c f") #'my/dired-create-file)
  (define-key dired-mode-map (kbd "C-c d") #'my/dired-create-directory)
  (define-key dired-mode-map (kbd "C-s")   #'my/dired-search))

(provide 'dired-config)
;;; dired-config.el ends here