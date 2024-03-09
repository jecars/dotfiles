
;; -------------------- Keybinds --------------------

(map! :desc "Repeat" :g "C-." #'repeat)

(map! :leader :desc "Select Treemacs" :g "TAB" #'treemacs-select-window)

(after! tree-sitter
  (map! :leader :desc "Tree Sitter Highlight" :g "t h" #'tree-sitter-hl-mode))

(after! company
  (map! :leader :desc "Company Complete" :g "SPC" #'company-complete))

(after! god-mode
  (god-mode-all -1)
  (map! :desc "Toggle God Mode" :g "M-m" #'god-local-mode)
  (define-key god-local-mode-map (kbd ".") #'repeat)
  (define-key god-local-mode-map (kbd "C-x C-1") #'delete-other-windows)
  (define-key god-local-mode-map (kbd "C-x C-2") #'split-window-below)
  (define-key god-local-mode-map (kbd "C-x C-3") #'split-window-right)
  (define-key god-local-mode-map (kbd "C-x C-0") #'delete-window)

  (after! ace-window
    (define-key god-local-mode-map (kbd "C-x C-o") #'ace-window)))

(after! tab-bar
  (map! :desc "Toggle Tab Bar Mode" :g "C-x t C-0" #'tab-bar-mode)
  )
