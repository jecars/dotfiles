
;; -------------------- Look and Feel --------------------

(setq doom-theme
      ;; 'doom-one
      ;; 'doom-old-hope
      ;; 'doom-1337
      ;; 'leuven-dark
      ;; 'doom-xcode
      'darktooth)


;; https://vidhukant.com/blog/2022/making-your-own-doom-emacs-theme/
(custom-theme-set-faces! 'doom-1337
  '(mode-line :background "#476685")
  '(mode-line-inactive :background "#242628")
  '(hl-line :background "#252526"))


(custom-theme-set-faces! 'darktooth
    ;; all colours come from darktooh theme unless specified

    ;; so that (text-scale-adjust) scales properly
    '(line-number :inherit default :foreground "#7C6F64" "#767676")
    '(line-number-current-line :inherit default :foreground "#7C6F64" "#767676")

    ;; tab bar
    '(tab-bar-tab :background "#3a3a3a" "#3a3a3a")
    '(tab-bar-tab-inactive :background "#1c1c1c" "#1c1c1c")
    '(tab-bar :background "#1c1c1c" "#1c1c1c")

    ;; org mode
    '(org-block :background "#232323" "#262626")

    ;; modeline icons bold
    '(success :foreground "#B8BB26" "#73AF00" :bold nil)
    '(error :foreground "#FB4933" "#d75f5f" :bold nil))
