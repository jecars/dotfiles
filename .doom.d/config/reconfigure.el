
;; -------------------- Reconfigure Packages --------------------

(global-tree-sitter-mode t) ;; for some reason it didnt work inside the after! block
(after! (tree-sitter)
  ;; (global-tree-sitter-mode t)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(after! treemacs
  (treemacs-follow-mode 1))

(after! company
  (setq company-minimum-prefix-length 1))
