;;; org-config.el --- Org mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Readable Org with org-indent-mode enabled, but without ugly mouse selection.
;; Includes agenda/capture/refile/tags/habits/babel + sane editing defaults.

;;; Code:

(require 'org)

;; ============================================================
;; BASIC ORG SETTINGS
;; ============================================================

(setq org-startup-align-all-tables t
      org-startup-shrink-all-tables t
      org-log-reschedule 'time
      org-log-done nil
      org-log-into-drawer nil
      org-log-state-notes-into-drawer nil
      org-use-fast-todo-selection 'expert

      ;; nicer editing defaults
      org-return-follows-link t
      org-hide-emphasis-markers t
      org-pretty-entities t
      org-ellipsis " ▾"
      org-image-actual-width '(300)
      org-fontify-quote-and-verse-blocks t
      org-fontify-whole-heading-line t
      org-fontify-done-headline t)

(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "<f6>")  #'org-capture)

;; ============================================================
;; INDENTATION (READABILITY) + FIX MOUSE SELECTION LOOK
;; ============================================================
;; Keep org-indent-mode (virtual indentation) but make selection behave nicer.

(setq org-startup-indented t) ;; enables org-indent-mode on open

(defun my/org-visual-selection ()
  "Make selection feel normal even with org-indent-mode."
  ;; This makes selection operate on *visual lines* (what you see),
  ;; which avoids the ugly virtual-indent highlight effect.
  (setq-local line-move-visual t)
  (setq-local truncate-lines nil))

(add-hook 'org-mode-hook #'my/org-visual-selection)

;; Optional: don’t auto-copy on mouse drag (can feel less annoying)
(setq mouse-drag-copy-region nil)


;;
(use-package org-transclusion
  :ensure t
  :after org)

(with-eval-after-load 'org
  (require 'org-transclusion)
  (add-hook 'org-mode-hook #'org-transclusion-mode))
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
;; AUTOSAVE — ORG ONLY
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
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)

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

(setq mouse-drag-copy-region nil)   ;; don’t auto-copy
(setq select-active-regions t)
(setq org-use-sub-superscripts nil)

(provide 'org-config)
;;; org-config.el ends here
