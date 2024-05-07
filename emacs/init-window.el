
;; -------------------- Window Configurations --------------------


;; (custom-set-variables 
;;  '(switch-to-buffer-in-dedicated-window 'pop)
;;  '(switch-to-buffer-obey-display-actions t))

;; (defun display-buffer-same-window (buffer alist)
;;   (unless (or (cdr (assq 'inhibit-same-window alist))
;;               (window-minibuffer-p)
;;               (window-dedicated-p))
;;     (window--display-buffer buffer (selected-window) 'reuse alist)))

;; (defun mp-split-below (arg)
;;   "Split window below from the parent or from root with ARG."
;;   (interactive "P")
;;   (split-window (if arg (frame-root-window)
;;                   (window-parent (selected-window)))
;;                 nil 'below nil))

;; (defun mp-split-right (arg)
;;   "Split window right from the parent or from root with ARG."
;;   (interactive "P")
;;   (split-window (if arg (frame-root-window)
;;                   (window-parent (selected-window)))
;;                 nil 'right nil))


;; (defun mp-toggle-window-dedication ()
;;   "Toggles window dedication in the selected window."
;;   (interactive)
;;   (set-window-dedicated-p (selected-window)
;;      (not (window-dedicated-p (selected-window)))))

;; ;; M-x window-toggle-side-window
;; ;; you can bind the above command to toggle side windows!

;; (add-to-list 'display-buffer-alist
;;  '("\\*Help\\*"
;;    (display-buffer-reuse-window display-buffer-pop-up-window)
;;    (inhibit-same-window . t)))

;; ;; Couldnt figure this out
;; (add-to-list 'display-buffer-alist
;;    '("\*Calc Trail\*"
;;      (display-buffer-in-atom-window)
;; 	 (get-buffer-window "*Calculator*")
;; 	 (side . right)))

;; ;; left, top, right, bottom
;; (setq window-sides-slots '(1 0 0 1))

;; (add-to-list 'display-buffer-alist
;;           `(,(rx (| "*compilation*" "*grep*" "*shell*" "*eshell*"))
;;             display-buffer-in-side-window
;;             (side . bottom)
;;             (slot . 0)
;;             (window-parameters . ((no-delete-other-windows . t)))
;;             (window-height . 10)))

;; (add-to-list 'display-buffer-alist
;;   '("\\*e?shell\\*" display-buffer-in-direction
;;     (direction . bottom)
;;     (window . root)
;;     (window-height . 0.3)))


;; (keymap-global-set "C-c s" 'window-toggle-side-windows)


(use-package window
  :custom
  (display-buffer-alist
   ;; bottom slot -1
   '(("\\*e?shell\\*"
	  (display-buffer-in-side-window)
	  (window-height . 0.25)
	  (side . bottom)
	  (slot . -1)
	  (window-parameters . ((no-delete-other-windows . t))))
	 ;; bottom slot 0
	 ("\\*\\([Hh]elp\\|Warnings\\|Messages\\|Backtrace\\)\\*"
	  (display-buffer-in-side-window)
	  (window-height . 0.25)
	  (side . bottom)
	  (slot . 0))
	 ;; bottom slot 1
	 ("\\*Faces\\*"
	  (display-buffer-in-side-window)
	  (window-height . 0.25)
	  (side . bottom)
	  (slot . 1))
	 ;; ("\\*dirvish.*\\*"
	 ;;  (display-buffer-in-side-window)
	 ;;  (display-buffer-reuse-window)
	 ;;  (side . left)
	 ;;  (slot . -1))
	 ))
  :bind (("C-c n" . next-buffer)
		 ("C-c p" . previous-buffer)
		 ("C-c s" . window-toggle-side-windows)))

(use-package winner
  :straight t
  :hook (after-init . winner-mode)
  :bind (("C-c P" . winner-undo)
		 ("C-c N" . winner-redo)))
