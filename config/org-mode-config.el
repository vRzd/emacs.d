;; ============================
;; ORG MODE CONFIGURATIONS
;; ============================
(setq org-startup-align-all-tables t)
(setq org-startup-shrink-all-tables t)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-<right>") 'org-metaright))

(setq org-log-reschedule 'time)

(setq org-agenda-files '("/Users/vdrozd/Dropbox/org/daily.org"
                         "/Users/vdrozd/Dropbox/org/work.org"
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
(setq org-capture-templates
      '(("t" "Air Travel" entry
         (id "travel")
         (file "~/.emacs.d/templates/research-air-travel.tpl")
         :prepend t
         :immediate-finish t)))

;; Function to start Org Capture for air travel
(defun start-air-travel-capture ()
  "Start Org Capture for air travel template."
  (interactive)
  (org-capture nil "t"))
;; Bind the function to a key, for example, F7
(global-set-key (kbd "<f7>") 'start-air-travel-capture)

(global-set-key (kbd "<f6>") 'org-capture)

;; Clock
(evil-define-key 'insert org-mode-map (kbd "C-c C-x .") 'org-time-stamp)
