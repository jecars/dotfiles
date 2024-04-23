
;; -------------------- Scrolling --------------------

;; scroll the compilation buffer
(setq compilation-scroll-output t)
(setq compilation-scroll-output 'first-error)

;; scroll with mouse
;; (use-package! good-scroll
;;   :init
;;   (good-scroll-mode t))

;; (use-package! smooth-scrolling
;;   :init
;;   (smooth-scrolling-mode 1)
;;   (setq smooth-scroll-margin 6))



;; (progn
;;   ;; unbind `scroll-up-command` from `C-v`
;;   (global-unset-key [?\C-v])
;;   ;; bind the function you want to `C-v`
;;   (global-set-key [?\C-v]
;;                   #'(lambda ()
;;                       (interactive)
;;                       (let ((number-of-lines (window-total-height))) ; scroll N lines
;;                         ;; execute what you want:
;;                         ;; “one line at a time”
;;                         (dotimes (_ number-of-lines)
;;                           ;; Scroll up 1 line,
;;                           ;; but the cursor moves on the screen too.
;;                           ;; (scroll-up-line)
;;                           (pixel-scroll-up)
;;                           ;; Move the cursor down 1 line so that
;;                           ;; the cursor doesn't appear to have changed position.
;;                           (forward-line)
;;                           (redisplay)
;;                           (sleep-for (/ 0.01 number-of-lines))
;;                           )))); spend N seconds scrolling one line
;;   (global-unset-key [?\M-v])
;;   (global-set-key [?\M-v]
;;                   #'(lambda ()
;;                       (interactive)
;;                       (let ((number-of-lines (window-total-height))) ; scroll N lines
;;                         ;; execute what you want:
;;                         ;; “one line at a time”
;;                         (dotimes (_ number-of-lines)
;;                           ;; Scroll up 1 line,
;;                           ;; but the cursor moves on the screen too.

;;                           ;; (scroll-down-line)
;;                           (pixel-scroll-down)

;;                           ;; Move the cursor down 1 line so that
;;                           ;; the cursor doesn't appear to have changed position.
;;                           (forward-line -1)
;;                           (redisplay)
;;                           (sleep-for (/ 0.01 number-of-lines))
;;                           )))))

;; (progn
;;   ;; unbind `scroll-up-command` from `C-v`
;;   (global-unset-key [?\C-v])
;;   ;; bind the function you want to `C-v`
;;   (global-set-key [?\C-v]
;;                   #'(lambda ()
;;                       (interactive)
;;                       (let ((number-of-lines (window-total-height))) ; scroll N lines
;;                         ;; execute what you want:
;;                         ;; “one line at a time”
;;                         ;; (pixel-scroll-up)
;;                         (good-scroll-up-full-screen)
;;                         ;; (forward-line number-of-lines)
;;                         )
;;                       )
;;                   ); spend N seconds scrolling one line
;;   (global-unset-key [?\M-v])
;;   (global-set-key [?\M-v]
;;                   #'(lambda ()
;;                       (interactive)
;;                       (let ((number-of-lines (window-total-height))) ; scroll N lines
;;                         ;; (pixel-scroll-pixel-down 500)
;;                         (good-scroll-down-full-screen)
;;                         )
;;                       )
;;                   ))



(setq scroll-margin 4)
(pixel-scroll-precision-mode)
(setq pixel-scroll-precision-large-scroll-height 40.0)
