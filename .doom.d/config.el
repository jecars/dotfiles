;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys

(when (eq system-type 'windows-nt)
  (load! "config/windows-nt.el"))

(load! "config/scrolling.el")
(load! "config/defaults.el")
(load! "config/keybinds.el")
(load! "config/lib.el")
(load! "config/reconfigure.el")
(load! "config/theme.el")
(load! "config/dashboard.el")

(load! "config/lang/lsp.el")
(load! "config/lang/debugging.el")
(load! "config/lang/remote.el")
(load! "config/lang/lang.el")
