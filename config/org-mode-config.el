;; ============================
;; ORG MODE CONFIGURATIONS
;; ============================
(setq org-startup-align-all-tables t)
(setq org-startup-shrink-all-tables t)


(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-<right>") 'org-metaright)
  (define-key org-mode-map (kbd "C-c C-e") nil))

(setq org-log-reschedule 'time)

(global-set-key "\C-ca" 'org-agenda)


(setq org-agenda-files '("/Users/vdrozd/Dropbox/org/daily.org"
                         "/Users/vdrozd/Dropbox/org/work.org"
                         "/Users/vdrozd/Dropbox/org/it.org"
                         "/Users/vdrozd/Dropbox/org/personal.org"))
(setq auto-save-default t)     ;; Enable autosave
(setq auto-save-interval 10)   ;; Autosave every 20 keystrokes
(setq auto-save-timeout 10)    ;; Autosave after 20 seconds of idle time

(defun my-org-mode-autosave-settings ()
  (auto-save-mode 1))

(add-hook 'org-mode-hook 'my-org-mode-autosave-settings)


(setq org-refile-use-outline-path 'file)

(setq org-refile-allow-creating-parent-nodes t)


(setq org-refile-targets '((nil :maxlevel . 3)
                           (org-agenda-files :maxlevel . 3)))

;; Disable automatic timestamp logging in Org mode
(setq org-log-done 'nil)
(setq org-log-into-drawer 'nil)

;; Remove any custom hooks that might add timestamps
;;(remove-hook 'org-after-todo-state-change-hook 'org-add-log-setup)

;; Function to remove existing timestamp notes in Org files
;;(defun remove-org-timestamps ()
;;  "Remove all lines with Org timestamps in the current buffer."
;;  (interactive)
;;  (goto-char (point-min))
;;  (while (re-search-forward "- Note taken on \\[.*\\]" nil t)
;;   (replace-match "")))

;; Optionally, bind the function to a key for easy access
;;(global-set-key (kbd "C-c r") 'remove-org-timestamps)

;; Ensure Org mode is loaded
(require 'org)

;; Define Org Capture Templates
;(setq org-capture-templates
;      '(("t" "Air Travel" entry
;         (id "travel")
;         (file "~/.emacs.d/templates/research-air-travel.tpl")
;         :prepend t
;         :immediate-finish t)))

;; Function to start Org Capture for air travel
;(defun start-air-travel-capture ()
;  "Start Org Capture for air travel template."
;  (interactive)
;  (org-capture nil "t"))
;; Bind the function to a key, for example, F7
;(global-set-key (kbd "<f7>") 'start-air-travel-capture)

(global-set-key (kbd "<f6>") 'org-capture)

;; Clock
(evil-define-key 'insert org-mode-map (kbd "C-c C-x .") 'org-time-stamp)

;; Define global tags with shortcuts
(setq org-tag-alist
      '((:startgroup)
;;        ("@Alina" . ?a)   ;; Shortcut 'a' for @Alina
;;        ("@John" . ?j)    ;; Shortcut 'j' for @John
        ("@Project" . ?p) ;; Shortcut 'p' for @ProjectX
        (:endgroup)))

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)


(setq org-habit-graph-column 50) ;; Position of the habit graph in agenda view
(setq org-habit-preceding-days 28) ;; Number of days to display in the habit graph
(setq org-habit-following-days 7) ;; Number of future days to display in the habit graph

;; Enable Org Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)))  ;; Add SQL support

(setq org-src-fontify-natively t) ;; Enable syntax highlighting
(setq org-src-tab-acts-natively t) ;; Make TAB work in code blocks

;; Enable Org Babel languages, including Java
;;#+BEGIN_SRC java :classname HelloWorld

(org-babel-do-load-languages
 'org-babel-load-languages
 '((java . t))) ;; Add Java support

;; Enable Org Babel languages, including Java
(org-babel-do-load-languages
 'org-babel-load-languages
 '((java . t))) ;; Add Java support

;; Enable Org Babel languages, including Shell
;; #+BEGIN_SRC sh or #+BEGIN_SRC sh :var name="Alice"
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t))) ;; Add Shell support



(setq org-use-fast-todo-selection 'expert)

