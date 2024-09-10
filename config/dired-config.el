;; ============================
;; DIRED MODE 
;; ============================

(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(setq delete-by-moving-to-trash t)
