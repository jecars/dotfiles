
;; -------------------- Defaults --------------------

(setq user-full-name "Joshua Carrasco"
      user-mail-address "mail@joshuacarrasco.com")

(setq-default delete-by-moving-to-trash t)

(setq display-line-numbers-type t)

(setq org-directory "~/org/")

;; frame title
(set-frame-name "emacs btw")

;; dont ask me
(setq confirm-kill-emacs nil
      confirm-kill-processes nil)

(setq-default tab-width 4)

;; so cross program copy paste will add to kill ring
(setq save-interprogram-paste-before-kill t)

;; default font was "Source Code Pro"
;; font installed from https://github.com/protesilaos/iosevka-comfy
(set-frame-font "Iosevka Comfy Wide 11" nil t)
;; (set-frame-font "Source Code Pro 11" nil t)

;; set initial size
(when window-system
  (set-frame-size (selected-frame) 136 50))
