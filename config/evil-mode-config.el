;; ============================
;; EVIL MODE CONFIGURATION
;; ============================
(unless (package-installed-p 'evil)
  (package-install 'evil))
(require 'evil)
(evil-mode 1)

;; Additional Evil configurations
(setq evil-want-C-i-jump nil)
(setq evil-want-fine-undo t)

;; Conditional Key Binding for Evil
(when evil-want-C-i-jump
  (define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward))

;; Set the undo system for Evil with 'undo-tree'
(unless (package-installed-p 'undo-tree)
  (package-refresh-contents)
  (package-install 'undo-tree))
(require 'undo-tree)
(global-undo-tree-mode)
(evil-set-undo-system 'undo-tree)

;; Directory where you want to save undo files
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))


;; Disable Evil in specific modes
(with-eval-after-load 'evil
  (evil-set-initial-state 'nov-mode 'emacs)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'Buffer-menu-mode 'emacs)
  (evil-set-initial-state 'org-agenda-mode 'emacs)
  (evil-set-initial-state 'calendar-mode 'emacs))

;; ============================
;; DIRED MODE CONFIGURATION
;; ============================
;; Automatically hide details in Dired mode
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; Custom function to disable Evil in Dired mode explicitly
(defun disable-evil-in-dired ()
  (evil-emacs-state))
(add-hook 'dired-mode-hook 'disable-evil-in-dired)


;; !!! TODO moving to init.el
;; Setting for moving deleted files to trash
(setq delete-by-moving-to-trash t)

;; Vertico Directory Tidy
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

;; ============================
;; ADDITIONAL CUSTOMIZATIONS
;; ============================
;; Add here any additional customizations or configurations

(evil-define-key 'normal org-mode-map
  (kbd "RET") 'org-open-at-point
  (kbd "TAB") 'org-cycle)

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-_") nil)
  (define-key evil-visual-state-map (kbd "C-_") nil)
  (define-key evil-insert-state-map (kbd "C-_") nil)
  (define-key evil-motion-state-map (kbd "C-_") nil))

;; Open your .emacs or init.el file and add the following line
(define-key isearch-mode-map (kbd "s-v") 'isearch-yank-kill)
