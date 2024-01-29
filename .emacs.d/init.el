;; ---------- Startup Performace Optimizations ----------
(setq gc-cons-threshold (* 50 1000 1000))
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; ---------- Change UI Elements ----------

;; (tool-bar-mode 1)
;;(menu-bar-mode 1)
(scroll-bar-mode -1)
(global-hl-line-mode 1) ;; highlight line
(setq ring-bell-function 'ignore)

;; ---------- X clipboard interactions ----------

(setq-default
  ;; mouse pastes at cursor not mouse position
  mouse-yank-at-point t

  ;; save copies from other program to kill ring so that
  ;; kills in emacs dont erase the selection
  save-interprogram-paste-before-kill t)

;; ---------- Change Defaults ----------

(setq-default
 
  ;; Set frame title
  frame-title-format (format "emacs btw")
 
  ;; Put backup files in  ~/.emacs.d/backups
  backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))

  ;; Do not autosave.
  auto-save-default nil)

;; Change all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; ---------- Load Files ----------

(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

;; ---------- Packages ----------

;; (require 'package)
(package-initialize)
(setq package-archive-priorities '(("gnu" . 10)
                                   ("melpa" . 5))
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://stable.melpa.org/packages/")
                         ("melpa-devel" . "https://melpa.org/packages/")))

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(use-package company
  :ensure t
  ;; :bind (:map
  ;; 	company-active-map
  ;; 	("C-n" . 'company-select-next))
  :config
  (setq company-idle-delay 0.0)
  (global-company-mode t))


(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status))

;; ---------- Look and feel ----------

(use-package ewal-spacemacs-themes
  :ensure t
  :config
  (setq spacemacs-theme-comment-bg nil
        spacemacs-theme-comment-italic nil)
  (load-theme 'spacemacs-dark t))

;; ---------- Other Hooks ----------

(add-hook 'emacs-startup-hook
	  (lambda ()
	    "Startup hook in init.el"

	    ;; Startup time optimizations
	    (setq
	     file-name-handler-alist default-file-name-handler-alist
	     gc-cons-threshold 800000)

	    ;; Display startup time
	    (message "Loaded in %s with %d garbage collections."
		     (format "%.2fs"
			     (float-time
			     (time-subtract after-init-time before-init-time)))
		     gcs-done)))


