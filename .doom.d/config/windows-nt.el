
;; -------------------- Windows Configs --------------------


;; use vs code font
(set-frame-font "Cascadia Code" nil t)

;; vterm doesnt work on windows. use eshell instead in packages.el!

(use-package window
  :custom
  (display-buffer-alist
   ;; bottom slot -1
   '(("\\*doom:eshell-popup:.*\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . -1)
      (window-parameters . ((no-delete-other-windows . t)))))))
