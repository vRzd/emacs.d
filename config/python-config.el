;; ============================
;; PYTHON CONFIGURATIONS
;; ============================

(use-package elpy
    :straight t
    :bind
    (:map elpy-mode-map
          ("C-M-n" . elpy-nav-forward-block)
          ("C-M-p" . elpy-nav-backward-block))
    :hook ((elpy-mode . flycheck-mode)
           (elpy-mode . (lambda ()
                          (set (make-local-variable 'company-backends)
                               '((elpy-company-backend :with company-yasnippet))))))
    :init
    (elpy-enable)
    :config
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    ; fix for MacOS, see https://github.com/jorgenschaefer/elpy/issues/1550
    (setq elpy-shell-echo-output nil)
    (setq elpy-rpc-python-command "python3")
    (setq elpy-rpc-timeout 2))


(use-package company
  :straight t
  :diminish company-mode
  :init
  (global-company-mode)
  :config
  ;; set default `company-backends'
  (setq company-backends
        '((company-files          ; files & directory
           company-keywords       ; keywords
           company-capf)  ; completion-at-point-functions
          (company-abbrev company-dabbrev)
          ))(use-package company-statistics
    :straight t
    :init
    (company-statistics-mode))(use-package company-web
    :straight t)(use-package company-try-hard
    :straight t
    :bind
    (("C-<tab>" . company-try-hard)
     :map company-active-map
     ("C-<tab>" . company-try-hard)))(use-package company-quickhelp
    :straight t
    :config
    (company-quickhelp-mode))
)

(use-package buftra
    :straight (:host github :repo "humitos/buftra.el"))(use-package py-pyment
    :straight (:host github :repo "humitos/py-cmd-buffer.el")
    :config
    (setq py-pyment-options '("--output=numpydoc")))(use-package py-isort
    :straight (:host github :repo "humitos/py-cmd-buffer.el")
    :hook (python-mode . py-isort-enable-on-save)
    :config
    (setq py-isort-options '("--lines=88" "-m=3" "-tc" "-fgw=0" "-ca")))(use-package py-autoflake
    :straight (:host github :repo "humitos/py-cmd-buffer.el")
    :hook (python-mode . py-autoflake-enable-on-save)
    :config
    (setq py-autoflake-options '("--expand-star-imports")))(use-package py-docformatter
    :straight (:host github :repo "humitos/py-cmd-buffer.el")
    :hook (python-mode . py-docformatter-enable-on-save)
    :config
    (setq py-docformatter-options '("--wrap-summaries=88" "--pre-summary-newline")))(use-package blacken
    :straight t
    :hook (python-mode . blacken-mode)
    :config
    (setq blacken-line-length '88))(use-package python-docstring
    :straight t
:hook (python-mode . python-docstring-mode))

;; Set variables before loading pyenv
(setq pyenv-installation-dir "/opt/homebrew/opt/pyenv"
      pyenv-show-active-python-in-modeline nil
      pyenv-modestring-prefix " "
      pyenv-modestring-postfix nil
      pyenv-use-alias 't)

;; Use-package directive for pyenv
(use-package pyenv
  :straight (:host github :repo "aiguofer/pyenv.el")
  :config
  (global-pyenv-mode)
   (defun pyenv-update-on-buffer-switch (prev curr)
      (if (string-equal "Python" (format-mode-line mode-name nil nil curr))
          (pyenv-use-corresponding)))
 (add-hook 'pyenv-mode-hook 'elpy-rpc-restart)
  (add-hook 'switch-buffer-functions 'pyenv-update-on-buffer-switch)
  )


(use-package python
  :hook (inferior-python-mode . fix-python-password-entry)
  :config
  (setq python-shell-interpreter "jupyter-console"
        python-shell-interpreter-args "--simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter-console")
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
  
  (defun fix-python-password-entry ()
    (push 'comint-watch-for-password-prompt comint-output-filter-functions))
)

(setq python-indent-offset 4)

(add-hook 'inferior-python-mode-hook
          (lambda () (setq comint-scroll-to-bottom-on-output t)))

