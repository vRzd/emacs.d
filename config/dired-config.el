;;; dired-config.el --- Complete Dired setup with Evil support

;; ===========================
;; Core Dired Setup
;; ===========================
(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom
  ;; Use GNU ls for macOS color support
  (insert-directory-program (if (executable-find "gls") "gls" insert-directory-program))
  ;; Use clean one-column listing with colors
  (dired-listing-switches "-1 --group-directories-first --color=always")
  ;; Move to trash instead of deleting
  (delete-by-moving-to-trash t)
  :hook ((dired-mode . dired-hide-details-mode) ;; Hide metadata
         (dired-mode . hl-line-mode)            ;; Highlight current line
         (dired-mode . evil-normalize-keymaps)) ;; Apply Evil keys
  :config
  ;; Optional: omit hidden files unless toggled
  (setq dired-omit-files "^\\.[^.].*"))

;; ===========================
;; Evil Integration
;; ===========================
(use-package evil-collection
  :after evil
  :custom
  (evil-collection-mode-list '(dired))
  :config
  (evil-collection-init))

;; ===========================
;; Dired Enhancements
;; ===========================
(use-package dired-single
  :after dired
  :config
  (evil-define-key 'normal dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-ranger
  :after dired
  :config
  (evil-define-key 'normal dired-mode-map
    "yy" 'dired-ranger-copy
    "p"  'dired-ranger-paste
    "x"  'dired-ranger-move))

(use-package dired-hide-dotfiles
  :after dired
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-define-key 'normal dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package all-the-icons-dired
  :after dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :after dired
  :config
  (setq dired-open-extensions
        '(("png" . "feh")
          ("jpg" . "feh")
          ("mp4" . "mpv")
          ("pdf" . "open"))))

;; ===========================
;; Custom File & Folder Creation
;; ===========================
(defun my/dired-create-file ()
  "Prompt for a new file name and create it in the current Dired directory."
  (interactive)
  (let* ((default-directory (dired-current-directory))
         (filename (read-string "New file name: "))
         (full-path (expand-file-name filename default-directory)))
    (if (file-exists-p full-path)
        (message "File already exists: %s" full-path)
      (write-region "" nil full-path)
      (revert-buffer)
      (message "Created file: %s" full-path))))

(defun my/dired-create-folder ()
  "Prompt for a new folder name and create it in the current Dired directory."
  (interactive)
  (let* ((default-directory (dired-current-directory))
         (foldername (read-string "New folder name: "))
         (full-path (expand-file-name foldername default-directory)))
    (if (file-exists-p full-path)
        (message "Folder already exists: %s" full-path)
      (make-directory full-path)
      (revert-buffer)
      (message "Created folder: %s" full-path))))

;; ===========================
;; Search / Jump by Filename
;; ===========================

(defun my/dired-jump-to-file ()
  "Jump to a file in the current Dired buffer by typing its name."
  (interactive)
  (let ((query (read-string "Jump to file: ")))
    (goto-char (point-min))
    (if (search-forward query nil t)
        (progn
          (beginning-of-line)
          (dired-move-to-filename)
          (message "Jumped to file: %s" query))
      (message "No file matching '%s' found" query))))

;; ===========================
;; Final Keybinding Setup
;; ===========================
(with-eval-after-load 'dired-open
  (define-key dired-mode-map (kbd "f") nil))

(with-eval-after-load 'evil-collection
  (with-eval-after-load 'dired
    (evil-define-key 'normal dired-mode-map
      "f" #'my/dired-create-file
      "+" #'my/dired-create-folder
      "/" #'dired-isearch-filenames   ;; built-in filename search
      "s" #'my/dired-jump-to-file)))  ;; manual search command

(provide 'dired-config)
;;; dired-config.el ends here
