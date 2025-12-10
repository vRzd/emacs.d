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

(setq org-agenda-custom-commands
      '(("c" "Tags in Current File"
         tags "" ;; Search all tags
         ((org-agenda-files (list buffer-file-name))))))

(setq org-agenda-files '("/Users/vdrozd/Dropbox/org/daily.org"
                         "/Users/vdrozd/Dropbox/org/work.org"
                         "/Users/vdrozd/Dropbox/org/it.org"
                         "/Users/vdrozd/Dropbox/org/personal.org"))

(with-eval-after-load 'evil
  (evil-define-key 'normal org-agenda-mode-map
    (kbd "v m") #'org-agenda-month-view))

;; ============================
;; ORG AGENDA VIEW OPTIMIZATION
;; ============================

(setq org-agenda-span 'week)                ;; show one week at a time
(setq org-agenda-start-on-weekday 1)        ;; Monday start
(setq org-agenda-include-diary nil)
(setq org-agenda-show-all-dates t)
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-start-with-log-mode nil)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
(setq org-agenda-skip-timestamp-if-done t)
(setq org-agenda-include-deadlines t)
(setq org-agenda-remove-times-when-in-prefix nil)
(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " %i %-12:c")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))

;; highlight current day
(setq org-agenda-day-face-function
      (lambda (date)
        (if (org-agenda-today-p date)
            'org-agenda-date-today
          'org-agenda-date)))

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")  ;; time, category, text
        (todo . " %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))

(setq org-agenda-use-time-grid nil)


;; ----------------------------
;; Autosave for Org buffers
;; ----------------------------
(setq auto-save-default t)
(setq auto-save-interval 10)
(setq auto-save-timeout 10)

(defun my-org-mode-autosave-settings ()
  (auto-save-mode 1))
(add-hook 'org-mode-hook 'my-org-mode-autosave-settings)

;; ----------------------------
;; Refile setup
;; ----------------------------
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes t)
(setq org-refile-targets '((nil :maxlevel . 3)
                           (org-agenda-files :maxlevel . 3)))

;; ----------------------------
;; Logging and drawers
;; ----------------------------
(setq org-log-done nil)
(setq org-log-into-drawer nil)

;; ----------------------------
;; Capture
;; ----------------------------
(global-set-key (kbd "<f6>") 'org-capture)

;; ----------------------------
;; Clock
;; ----------------------------
(evil-define-key 'insert org-mode-map (kbd "C-c C-x .") 'org-time-stamp)

;; ----------------------------
;; Tags
;; ----------------------------
(setq org-tag-alist
      '((:startgroup)
        ("@Project" . ?p)
        (:endgroup)))

;; ----------------------------
;; Habits
;; ----------------------------
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 50)
(setq org-habit-preceding-days 28)
(setq org-habit-following-days 7)

;; ----------------------------
;; Babel Languages
;; ----------------------------
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)
   (java . t)
   (shell . t)))

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(setq org-use-fast-todo-selection 'expert)

;; ============================================================
;; EVIL + macOS clipboard integration for Org and all buffers
;; ============================================================

;; Ensure clipboard sync globally
(setq select-enable-clipboard t)
(setq save-interprogram-paste-before-kill t)

(defun my/copy-to-macos-clipboard (text &optional _push)
  "Copy TEXT to the macOS clipboard."
  (when (eq system-type 'darwin)
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max) "pbcopy"))))

(defun my/paste-from-macos-clipboard ()
  "Paste text from the macOS clipboard."
  (when (eq system-type 'darwin)
    (shell-command-to-string "pbpaste")))

(setq interprogram-cut-function 'my/copy-to-macos-clipboard)
(setq interprogram-paste-function 'my/paste-from-macos-clipboard)

;; Custom Evil visual yank that also syncs with macOS clipboard
(defun my/evil-yank-to-macos-clipboard (beg end &optional _type _register _yank-handler)
  "Yank to Evil registers and macOS clipboard."
  (interactive "r")
  (evil-yank beg end _type _register _yank-handler)
  (let ((text (buffer-substring-no-properties beg end)))
    (my/copy-to-macos-clipboard text)))

(with-eval-after-load 'evil
  (define-key evil-visual-state-map (kbd "y") #'my/evil-yank-to-macos-clipboard))

(provide 'org-config)
;;; org-config.el ends here

;; ============================================================
;; ORG MODE – TAB toggles ONLY the current heading (no jumping)
;; ============================================================

(with-eval-after-load 'org

  ;; Disable global cycling on TAB
  (setq org-cycle-global-at-bob nil)
  (setq org-cycle-global-at-eob nil)
  (setq org-cycle-include-plain-lists nil)

  ;; Our perfect toggle: always fold/unfold ONLY the current heading
  (defun my/org-tab-toggle ()
    "Toggle ONLY the current heading visibility, without jumping."
    (interactive)
    (save-excursion
      (end-of-line)
      (org-cycle)))

  ;; Bind TAB in org buffers
  (define-key org-mode-map (kbd "TAB") #'my/org-tab-toggle)
  (define-key org-mode-map (kbd "<tab>") #'my/org-tab-toggle)
)

;; ============================================================
;; EVIL MODE – ensure TAB works the same in normal state
;; ============================================================

(with-eval-after-load 'evil
  (evil-define-key 'normal org-mode-map
    (kbd "TAB") #'my/org-tab-toggle)
  (evil-define-key 'normal org-mode-map
    (kbd "<tab>") #'my/org-tab-toggle))
