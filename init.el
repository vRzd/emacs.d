
;; ============================
;; PACKAGE INITIALIZATION
;; ============================
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)  
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(package-initialize)

(setq exec-path-from-shell-shell-name "/bin/zsh")

(unless (package-installed-p 'use-package)
  (package-refresh-content)
  (package-install 'use-package))

;; STRAIGHT.EL
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)


(eval-when-compile
  (require 'use-package))

(package-install 'exec-path-from-shell)
(exec-path-from-shell-initialize)


(use-package all-the-icons)

;; nerd-icons-install-fonts
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))



;; Path to your emacs directory
(add-to-list 'load-path "~/.emacs.d/config/")

;; Load configuration files
(load "ui-config.el")
(load "evil-mode-config.el")
(load "python-config.el")
(load "org-mode-config.el")
(load "dired-config.el")



;; ============================
;; PACKAGE SPECIFIC CONFIGURATIONS
;; ============================
(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))

(use-package vertico
  :config
  (vertico-mode))

(use-package term
  :config
  (setq explicit-shell-file-name "zsh")
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))



;; ============================
;; MISC CONFIGURATIONS
;; ============================
(recentf-mode 1)
(setq history-length 25)
(setq savehist-mode 1)
(setq save-place-mode 1)
(setq ring-bell-function 'ignore)

;; Enable mouse wheel tilt scroll
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction (if (eq system-type 'darwin) t nil))
;; Set mac-mouse-wheel-smooth-scroll only for macOS (Darwin)
(when (eq system-type 'darwin)
  (setq mac-mouse-wheel-smooth-scroll t))

;; vertio
;;(vertico-mode 1)

;; marginalia
(marginalia-mode 1)

;; Automatically hide the detailed listing when visiting a Dired
;; buffer.  This can always be toggled on/off by calling the
;; `dired-hide-details-mode' interactively with M-x or its keybindings
;; (the left parenthesis by default).
(add-hook 'dired-mode-hook #'dired-hide-details-mode)


(setq use-short-answers t)


;;(add-hook 'org-mode-hook (lambda () (olivetti-mode 1)))
