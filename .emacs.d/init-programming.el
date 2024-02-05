
;; -------------------- LSP Client --------------------


(use-package eglot
  :ensure t
  :defer t
  :bind (:map eglot-mode-map
			  ("C-c c" .  compile)
			  ("C-c ]" . flymake-goto-next-error)
			  ("C-c [" . flymake-goto-prev-error)
			  ("C-c f" . eglot-format-buffer)
			  ("C-c c" . compile)
			  ("C-c ]" . flymake-goto-next-error)
			  ("C-c [" . flymake-goto-prev-error)
			  ("C-c r" . eglot-rename)
			  ("C-c a" . eglot-code-actions)
			  ("C-c <f5>" . eglot-reconnect))
  :config
  (superword-mode)
  (flyspell-prog-mode)
  (hs-minor-mode)
  (set-fill-column 88))


;; -------------------- Misc Configs --------------------


(use-package eldoc
  :ensure t
  :defer t
  :config
  (setq eldoc-idle-delay 0.0))


;; Compilation window
;; (setq compilation-window-height 8)
;; (setq compilation-messages-start nil)
;; (setq compilation-message-face nil)


;; -------------------- Python --------------------


(use-package pyvenv
  :ensure t
  :after (python-mode)
  :config
  (pyvenv-mode t)

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))


(use-package python-mode
  :ensure t
  :hook ((python-mode . my-python-mode-hook)
		 (python-mode . eglot-ensure))
  :init
  (defun my-python-mode-hook ()
	(keymap-local-set "C-c v" 'pyvenv-activate)
	(keymap-local-set "C-c V" 'pyvenv-deactivate)
	(keymap-local-set "C-c v" (lambda ()
								(interactive)
								(pyvenv-activate "env")))

	(if (not (string-match "python3" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "python3")))
  :config
  (setq-default eglot-workspace-configuration
                '((:pylsp . (:configurationSources ["flake8"]
                             :plugins (
                                       :pycodestyle (:enabled t)
                                       :mccabe (:enabled :json-false)
                                       :pyflakes (:enabled :json-false)
                                       :flake8 (:enabled :json-false
                                                :maxLineLength 88)
                                       :ruff (:enabled t
                                              :lineLength 88)
                                       :pydocstyle (:enabled :json-false
                                                    :convention "numpy")
                                       :yapf (:enabled t)
                                       :autopep8 (:enabled :json-false)
                                       :black (:enabled t
                                               :line_length 88
                                               :cache_config t)))))))


;; -------------------- Golang --------------------


(use-package go-mode
  :ensure t
  :hook ((go-mode . my-go-mode-hook)
		 (go-mode . eglot-ensure))
  :init
  (defun my-go-mode-hook ()
    (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go run"))))

;; -------------------- Rust --------------------


(use-package rust-mode
  :ensure t
  :hook ((rust-mode . my-rust-mode-hook)
		 (rust-mode . eglot-ensure))
  :init
  (defun my-rust-mode-hook ()
    (if (not (string-match "rust" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "cargo run"))))


;; -------------------- C --------------------


(use-package c-mode

  :hook ((c-mode . my-c-mode-hook)
		 (c-mode . eglot-ensure))
  :init
  (defun my-c-mode-hook ()
    (if (not (string-match "clang" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "clang"))))


;; -------------------- C++ --------------------


(use-package c++-mode

  :hook ((c++-mode . my-c++-mode-hook)
		 (c++-mode . eglot-ensure))
  :init
  (defun my-c++-mode-hook ()
    (if (not (string-match "clang++" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "clang++"))))
