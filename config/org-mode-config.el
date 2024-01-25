;; ============================
;; ORG MODE CONFIGURATIONS
;; ============================
(setq org-startup-align-all-tables t)
(setq org-startup-shrink-all-tables t)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-<right>") 'org-metaright))

(setq org-log-reschedule 'time)
