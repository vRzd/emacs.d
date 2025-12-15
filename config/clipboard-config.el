;;; clipboard-config.el --- macOS clipboard integration -*- lexical-binding: t; -*-

;; -------------------------------------
;; macOS clipboard
;; -------------------------------------
(when (eq system-type 'darwin)
  (setq select-enable-clipboard t
        save-interprogram-paste-before-kill t)

  (defun my/copy-to-macos-clipboard (text &optional _push)
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max) "pbcopy")))

  (defun my/paste-from-macos-clipboard ()
    (shell-command-to-string "pbpaste"))

  (setq interprogram-cut-function   #'my/copy-to-macos-clipboard
        interprogram-paste-function #'my/paste-from-macos-clipboard))

;; ----------------------------------
;; Yank to clipboard
;; ----------------------------------
(defun my/vterm-yank-to-clipboard ()
  "Yank selected region in `vterm-copy-mode` to system clipboard."
  (interactive)
  (when (use-region-p)
    (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
      (kill-new text)
      (if (display-graphic-p)
          (gui-set-selection 'CLIPBOARD text)
        ;; Terminal fallback with xclip/wl-copy
        (let ((proc (start-process "xclip" nil "xclip" "-selection" "clipboard")))
          (process-send-string proc text)
          (process-send-eof proc)))
      (deactivate-mark)
      (message "Copied to clipboard"))))

(with-eval-after-load 'vterm
  (define-key vterm-copy-mode-map (kbd "y") #'my/vterm-yank-to-clipboard)
  (define-key vterm-mode-map (kbd "C-c y") #'my/vterm-yank-to-clipboard))

(add-hook 'vterm-mode-hook
          (lambda ()
            (evil-define-key 'visual vterm-mode-map
              (kbd "y") #'my/vterm-yank-to-clipboard)))

(provide 'clipboard-config)