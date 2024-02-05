;; set GC high for performance on startup.
;; is changed by gcmh package later.
(setq gc-cons-threshold most-positive-fixnum)

;; startup time optimization
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; ---------- UI Stuff ----------



(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)

(setq-default
 inhibit-redisplay t
 inhibit-message t)

;; ---------- Hooks ----------

(add-hook 'emacs-startup-hook
	  (lambda ()
	    "Reset value for file-name-handler-alist"
	    ;; Startup time optimizations
	    (setq file-name-handler-alist default-file-name-handler-alist)))

(add-hook 'window-setup-hook
	  (lambda ()
	    (setq-default
	     inhibit-redisplay nil
	     inhibit-message nil)
	    (redisplay)

	    (message "Loaded in %s with %d garbage collections."
		     (format "%.2fs"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))


;; -------------------- Package Stuff --------------------
;; (setq package-enable-at-startup nil)
