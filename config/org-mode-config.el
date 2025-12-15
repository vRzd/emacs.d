;;; org-config.el --- Org mode configuration -*- lexical-binding: t; -*-

;; ============================================================
;; BASIC ORG SETTINGS
;; ============================================================

(setq org-startup-align-all-tables t
      org-startup-shrink-all-tables t
      org-log-reschedule 'time
      org-log-done nil
      org-log-into-drawer nil
      org-use-fast-todo-selection 'expert)

(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "<f6>")  #'org-capture)

;; ============================================================
;; ORG AGENDA
;; ============================================================

(setq org-agenda-files
      '("~/Dropbox/org/daily.org"
        "~/Dropbox/org/work.org"
        "~/Dropbox/org/it.org"
        "~/Dropbox/org/personal.org"))

(setq org-agenda-span 'week
      org-agenda-start-on-weekday 1
      org-agenda-window-setup 'current-window
      org-agenda-use-time-grid nil
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-timestamp-if-done t
      org-agenda-include-deadlines t)

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " %i %-12:c")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))

(setq org-agenda-custom-commands
      '(("c" "Tags in Current File"
         tags ""
         ((org-agenda-files (list buffer-file-name))))))

(setq org-agenda-day-face-function
      (lambda (date)
        (if (org-agenda-today-p date)
            'org-agenda-date-today
          'org-agenda-date)))

;; ============================================================
;; ORG MODE KEYBINDINGS
;; ============================================================

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-<right>") #'org-metaright)
  (define-key org-mode-map (kbd "C-c C-e") nil))

(with-eval-after-load 'evil
  (evil-define-key 'normal org-agenda-mode-map
    (kbd "v m") #'org-agenda-month-view)
  (evil-define-key 'insert org-mode-map
    (kbd "C-c C-x .") #'org-time-stamp))

;; ============================================================
;; AUTOSAVE — ORG ONLY (clean & explicit)
;; ============================================================

(defun my/org-enable-autosave ()
  (setq-local auto-save-default t)
  (auto-save-mode 1))

(add-hook 'org-mode-hook #'my/org-enable-autosave)

;; ============================================================
;; REFILE
;; ============================================================

(setq org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes t
      org-refile-targets '((nil :maxlevel . 3)
                           (org-agenda-files :maxlevel . 3)))

;; ============================================================
;; TAGS
;; ============================================================

(setq org-tag-alist
      '((:startgroup)
        ("@Project" . ?p)
        (:endgroup)))

;; ============================================================
;; HABITS
;; ============================================================

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)

(setq org-habit-graph-column 50
      org-habit-preceding-days 28
      org-habit-following-days 7)

;; ============================================================
;; BABEL
;; ============================================================

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)
   (java . t)
   (shell . t)))

(setq org-src-fontify-natively t
      org-src-tab-acts-natively t)

;; ============================================================
;; TAB — toggle ONLY current heading (no jumping)
;; ============================================================

(with-eval-after-load 'org
  (setq org-cycle-global-at-bob nil
        org-cycle-global-at-eob nil
        org-cycle-include-plain-lists nil)

  (defun my/org-tab-toggle ()
    "Toggle only the current Org heading."
    (interactive)
    (when (org-at-heading-p)
      (save-excursion
        (end-of-line)
        (org-cycle))))

  (define-key org-mode-map (kbd "TAB") #'my/org-tab-toggle)
  (define-key org-mode-map (kbd "<tab>") #'my/org-tab-toggle))

(with-eval-after-load 'evil
  (evil-define-key 'normal org-mode-map
    (kbd "TAB") #'my/org-tab-toggle)
  (evil-define-key 'normal org-mode-map
    (kbd "<tab>") #'my/org-tab-toggle))

(provide 'org-config)
;;; org-config.el ends here
