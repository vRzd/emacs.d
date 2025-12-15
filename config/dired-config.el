;;; dired-config.el --- Complete Dired setup with Evil support -*- lexical-binding: t; -*-
;;; Commentary:
;; Clean, stable Dired setup (no straight.el, no missing packages).
;;; Code:

(require 'dired)

;; ===========================
;; Core Dired Setup
;; ===========================
(use-package dired
  :ensure nil        ;; built-in
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom
  ;; Use GNU ls for macOS color
  (insert-directory-program
   (or (executable-find "gls") insert-directory-program))

  ;; One column, directories first
  (dired-listing-switches "-alh --group-directories-first")

  ;; Move to trash instead of deleting
  (delete-by-moving-to-trash t)

  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode)
   (dired-mode . evil-normalize-keymaps)))

;; ===========================
;; Evil Integration
;; ===========================
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init 'dired))

;; ===========================
;; Replacement for dired-single
;; (use built-in navigation)
;; ===========================
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map
    "h" #'dired-up-directory
    "l" #'dired-find-file))

;; ===========================
;; dired-ranger (copy, paste, move)
;; ===========================
(use-package dired-ranger
  :ensure t
  :config
  (evil-define-key 'normal dired-mode-map
    "yy" #'dired-ranger-copy
    "p"  #'dired-ranger-paste
    "x"  #'dired-ranger-move))

;; ===========================
;; Dotfiles toggle
;; ===========================
(use-package dired-hide-dotfiles
  :ensure t
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-define-key 'normal dired-mode-map
    "H" #'dired-hide-dotfiles-mode))

;; ===========================
;; Icons
;; ===========================
(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

;; ===========================
;; dired-open (open with macOS)
;; ===========================
(use-package dired-open
  :ensure t
  :config
  (setq dired-open-extensions
        '(("png" . "open")
          ("jpg" . "open")
          ("mp4" . "open")
          ("pdf" . "open"))))

;; ===========================
;; Create File / Folder
;; ===========================
(defun my/dired-create-file ()
  "Create an empty file in Dired."
  (interactive)
  (let* ((default-directory (dired-current-directory))
         (name (read-string "New file: "))
         (path (expand-file-name name default-directory)))
    (if (file-exists-p path)
        (message "File exists: %s" path)
      (write-region "" nil path)
      (revert-buffer)
      (message "Created file: %s" path))))

(defun my/dired-create-folder ()
  "Create a folder in Dired."
  (interactive)
  (let* ((default-directory (dired-current-directory))
         (name (read-string "New folder: "))
         (path (expand-file-name name default-directory)))
    (if (file-exists-p path)
        (message "Folder exists: %s" path)
      (make-directory path)
      (revert-buffer)
      (message "Created folder: %s" path))))

;; ===========================
;; Search / Jump
;; ===========================
(defun my/dired-jump-to-file ()
  "Jump to a file by substring search."
  (interactive)
  (let ((query (read-string "Find: ")))
    (goto-char (point-min))
    (if (search-forward query nil t)
        (progn
          (beginning-of-line)
          (dired-move-to-filename)
          (message "Found: %s" query))
      (message "No match: %s" query))))

;; ===========================
;; Final Extra Keybindings
;; ===========================
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map
    "f" #'my/dired-create-file
    "+" #'my/dired-create-folder
    "/" #'dired-isearch-filenames
    "s" #'my/dired-jump-to-file))

(provide 'dired-config)
;;; dired-config.el ends here
