
;; -------------------- Remote Coding --------------------

;; https://coder.com/docs/v2/latest/ides/emacs-tramp
(after! (tramp lsp-mode)
  (progn
    (lsp-register-client ;; c/c++ remote LSP
     (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                      :major-modes '(c-mode c++-mode)
                      :remote? t
                      :server-id 'clangd-remote)) ;; you can pick whatever server-id you want
    (lsp-register-client ;; javascript remote LSP
     (make-lsp-client :new-connection (lsp-tramp-connection "typescript-language-server --stdio")
                      :major-modes '(rjsx-mode js2-mode typescript-mode)
                      :remote? t
                      :server-id 'ts-ls-remote)))
  )


(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
