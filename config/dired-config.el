;;; dired-config.el --- Clean, simple file-manager Dired -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs-native Dired configured to behave like a real file manager.
;; Same-buffer navigation, Backspace to go up, minimal keybindings.
;; Optimized for macOS and keyboard-driven workflows.

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
  ;; Prefer GNU ls on macOS (brew install coreutils)
  (insert-directory-program
   (or (executable-find "gls") insert-directory-program))

  ;; Human-readable, directories first
  (dired-listing-switches "-alh --group-directories-first")

  ;; Move files to trash instead of deleting
  (delete-by-moving-to-trash t)

  ;; Reuse buffers, no Dired buffer explosion
  (dired-kill-when-opening-new-dired-buffer t)

  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode)
   (dired-mode . auto-revert-mode)))

;; ============================================================
;; File-manager style navigation
;; ============================================================

(with-eval-after-load 'dired
  ;; Allow same-buffer navigation
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Enter directory / open file
  (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file)

  ;; Go up directory (Finder-style)
  ;; macOS Backspace == DEL
  (define-key dired-mode-map (kbd "DEL") #'dired-up-directory)
  (define-key dired-mode-map (kbd "^")   #'dired-up-directory)

  ;; Jump to directory of current buffer
  (define-key dired-mode-map (kbd "b")   #'dired-jump)

  ;; Refresh
  (define-key dired-mode-map (kbd "g")   #'revert-buffer))

;; ============================================================
;; Dotfiles toggle
;; ============================================================

(use-package dired-hide-dotfiles
  :ensure t
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
              ("H" . dired-hide-dotfiles-mode)))

;; ============================================================
;; Icons (optional, cosmetic)
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
      (dired-goto-file path)
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
      (dired-goto-file path)
      (message "Created directory: %s" name))))

;; ============================================================
;; Search / jump inside Dired
;; ============================================================

(defun my/dired-search ()
  "Search for a filename substring in the current Dired buffer."
  (interactive)
  (dired-isearch-filenames))

;; ============================================================
;; VTERM INTEGRATION (CORRECT)
;; ============================================================

(defun my/dired-open-vterm-here ()
  "Open a NEW vterm in the current Dired directory."
  (interactive)
  (let ((default-directory (dired-current-directory)))
    (vterm (generate-new-buffer-name "*vterm*"))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c t") #'my/dired-open-vterm-here))

;; ============================================================
;; Custom keybindings
;; ============================================================

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c f") #'my/dired-create-file)
  (define-key dired-mode-map (kbd "C-c d") #'my/dired-create-directory)
  (define-key dired-mode-map (kbd "C-s")   #'my/dired-search))

(provide 'dired-config)
;;; dired-config.el ends here
