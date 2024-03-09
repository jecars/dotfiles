
;; -------------------- Reconfigure Packages --------------------

(use-package! tree-sitter
  :init
  (global-tree-sitter-mode t)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  )

(after! treemacs
  (treemacs-follow-mode 1))

(after! company
  (setq company-minimum-prefix-length 1))
