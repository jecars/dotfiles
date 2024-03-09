
;; -------------------- LSP Configurations --------------------

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
    '(wgsl-mode . "wgsl"))

  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection "wgsl_analyzer")
                     :major-modes '(wgsl-mode)
                     :server-id 'wgsl-ls)))

;; LSPs
(after! python
  (setq! lsp-pylsp-plugins-black-enabled t))
