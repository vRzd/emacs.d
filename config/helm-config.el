(use-package helm
  :ensure t
  :init
  (require 'helm-config)
  :config
  (helm-mode 1)
  ;; Improve fuzzy matching behavior
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t)

  ;; If using Evil + general.el (like Doom Emacs)
  ;; Example keybindings for Helm in Evil-style
  :bind (
         ("M-x"     . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b"   . helm-mini)
         ("M-y"     . helm-show-kill-ring)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-r" . helm-recentf)))

;; Optional: for better projectile integration
(use-package helm-projectile
  :after (helm projectile)
  :config
  (helm-projectile-on))

