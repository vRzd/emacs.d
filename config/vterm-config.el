(defun my/vterm-yank-to-clipboard ()
  "Yank the current region in `vterm-copy-mode` directly to the system clipboard."
  (interactive)
  (when (use-region-p)
    (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
      (kill-new text) ;; Emacs kill-ring
      (when (display-graphic-p)
        (gui-set-selection 'CLIPBOARD text)) ;; system clipboard in GUI
      (unless (display-graphic-p)
        ;; terminal Emacs: use xclip/wl-copy
        (start-process "xclip" nil "xclip" "-selection" "clipboard" text))
      (deactivate-mark)
      (message "Copied to clipboard"))))

(with-eval-after-load 'vterm
  (define-key vterm-copy-mode-map (kbd "y") #'my/vterm-yank-to-clipboard))



(add-hook 'vterm-mode-hook
          (lambda ()
            (evil-define-key 'visual vterm-mode-map
              (kbd "y") #'my/vterm-yank-to-clipboard)))


(defun my/vterm-disable-mouse-scroll ()
  "Disable Emacs mouse scrolling in vterm to allow terminal apps (like vim) to handle it."
  (interactive)
  (setq-local mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq-local mouse-wheel-progressive-speed nil)
  (setq-local mouse-wheel-follow-mouse 't)
  (setq-local scroll-margin 0)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-step 1))

(add-hook 'vterm-mode-hook #'my/vterm-disable-mouse-scroll)
