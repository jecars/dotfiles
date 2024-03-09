
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

;; set initial size
(when window-system
  (set-frame-size (selected-frame) 136 50))

(setq-default tab-width 4)

;; so cross program copy paste will add to kill ring
(setq save-interprogram-paste-before-kill t)

