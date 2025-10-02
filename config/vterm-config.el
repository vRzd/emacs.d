

;; vterm terminal settings

;;(add-hook 'vterm-mode-hook (lambda () (evil-emacs-state)))
(add-hook 'vterm-mode-hook (lambda () (evil-local-mode -1)))


;(with-eval-after-load 'evil
;  (add-hook 'vterm-mode-hook
;            (lambda ()
;              (evil-local-mode -1)
;              (evil-escape-mode -1)
;              (setq-local evil-collection-mode nil))))
(defun my/disable-evil-in-vterm ()
  (when (derived-mode-p 'vterm-mode)
    (evil-local-mode -1)
    (evil-escape-mode -1)
    (setq-local evil-collection-mode nil)))

(add-hook 'vterm-mode-hook #'my/disable-evil-in-vterm)


(defun my/disable-evil-in-vterm ()
   (lambda ()
     (when (derived-mode-p 'vterm-mode)
       (evil-local-mode -1)
       (evil-escape-mode -1)
       (setq-local evil-collection-mode nil))))

(add-hook 'vterm-mode-hook #'my/disable-evil-in-vterm)

(add-hook 'vterm-mode-hook
          (lambda ()
            (evil-local-set-key 'insert (kbd "<delete>") 'vterm-send-delete)
            (evil-local-set-key 'normal (kbd "<delete>") 'vterm-send-delete)))

(use-package vterm
  :ensure t
  :config
  (define-key vterm-mode-map (kbd "M-<left>") 'vterm-send-M-b)
  (define-key vterm-mode-map (kbd "M-<right>") 'vterm-send-M-f)
  (define-key vterm-mode-map (kbd "<delete>") 'vterm-send-delete)
  (define-key vterm-mode-map (kbd "<kp-delete>") 'vterm-send-delete)
  (define-key vterm-mode-map (kbd "S-<backspace>") 'vterm-send-delete)
;  (define-key vterm-mode-map (kbd "C-r") nil)
;  (define-key vterm-mode-map (kbd "C-s") nil)
  (when (eq system-type 'darwin)
    (setq explicit-shell-file-name "zsh"))  ;; macOS zsh
  (when (eq system-type 'windows-nt)
    (setq explicit-shell-file-name "powershell.exe"))) ;; Windows



(add-hook 'vterm-mode-hook
          (lambda ()
            (define-key vterm-mode-map (kbd "<mouse-4>") nil) ; Forward scroll up
            (define-key vterm-mode-map (kbd "<mouse-5>") nil))) ; Forward scroll down


(use-package vterm-toggle
  :bind (("C-`" . vterm-toggle)))

(defun my/vterm-set-cursor-for-mode ()
  "Use a thin cursor in vterm copy mode, block cursor otherwise."
  (setq cursor-type (if (bound-and-true-p vterm-copy-mode)
                        'bar   ;; thin vertical bar in copy mode
                      'box))) ;; block cursor in normal mode

(add-hook 'vterm-copy-mode-hook #'my/vterm-set-cursor-for-mode)
(add-hook 'vterm-mode-hook #'my/vterm-set-cursor-for-mode)


(setq-default cursor-in-non-selected-windows nil)

(defun my/vterm-set-cursor-for-mode ()
  (setq cursor-type (if (bound-and-true-p vterm-copy-mode) 'bar 'box))
  (set-cursor-color (if (bound-and-true-p vterm-copy-mode) "orange" "white")))


(defvar my/vterm-pwd-result nil
  "Holds the output of remote pwd command.")

(defun my/vterm-get-remote-pwd ()
  "Run `pwd` inside vterm and get its result."
  (interactive)
  (when (eq major-mode 'vterm-mode)
    (let ((vterm-buffer (current-buffer)))
      (setq my/vterm-pwd-result nil)
      (let ((proc (get-buffer-process vterm-buffer)))
        (with-current-buffer vterm-buffer
          (goto-char (point-max))
          (let ((inhibit-read-only t))
            (vterm-send-string "pwd && echo __ENDPWD__\n"))
          (accept-process-output proc 0.2))
        (sleep-for 0.2) ;; give it a bit of time
        ;; Grab the last pwd line from buffer
        (with-current-buffer vterm-buffer
          (save-excursion
            (goto-char (point-max))
            (re-search-backward "__ENDPWD__" nil t)
            (forward-line -1)
            (setq my/vterm-pwd-result (string-trim (thing-at-point 'line t)))))))))

(defun open-remote-file-from-vterm ()
  "Open a remote file from vterm using TRAMP, starting at the remote shell's pwd."
  (interactive)
  (let ((remote-host "root@172.20.133.145"))
    (my/vterm-get-remote-pwd)
    (if my/vterm-pwd-result
        (let ((initial-path (concat "/ssh:" remote-host ":" my/vterm-pwd-result "/")))
          (find-file (read-file-name "Remote file: " initial-path)))
      (message "Could not detect remote PWD."))))


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


(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-c o r") #'open-remote-file-from-vterm))


(add-hook 'vterm-mode-hook
          (lambda ()
            (evil-define-key 'visual vterm-mode-map
              (kbd "y") #'my/vterm-yank-to-clipboard)))


(setq vterm-max-scrollback 100000)  ;; or any large number you prefer


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


