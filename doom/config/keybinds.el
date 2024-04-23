
;; -------------------- Keybinds --------------------

(map! :desc "Repeat" :g "C-." #'repeat)

;; default is +vertico/switch-workspace-buffer which can be accessed from leader-w-b which makes more sense.
(map! :g "C-x b" #'consult-buffer)

(map! :leader :desc "Select Treemacs" :g "TAB" #'treemacs-select-window)

(map!
 :after tree-sitter
 :leader
 :desc "Tree Sitter Highlight" :g "t h" #'tree-sitter-hl-mode)

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

(map! :after tab-bar :desc "Toggle Tab Bar Mode" :g "C-x t C-0" #'tab-bar-mode)

;; custom keybinds
(map! :leader
      (:prefix-map ("j" . "jecs")
       :desc "Stare" "s" #'jecs/stare
       :desc "Listening" "l" #'jecs/listening
       :desc "Open Phone" "p" #'scrcpy-open))

;; global keybinds
(map! :leader
      (:prefix-map ("x" . "global")
       :desc "Duplicate Line/Region" "d" #'duplicate-dwim))
