
;; -------------------- Setup Archives and Package Manager  --------------------


(setq package-archive-priorities '(("gnu" . 10)
                                   ("melpa" . 5))
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://stable.melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously

         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; use-package integration
(straight-use-package 'use-package)

;; -------------------- Configure Defaults --------------------


;; set initial size
(when window-system
  (set-frame-size (selected-frame) 125 45))

(setq confirm-kill-processes nil)

;; General stuff and defaults
;; tab size
(setq-default tab-width 4)

;; highlight current line
(global-hl-line-mode 1)

;; line numbering
;; (global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; no bell
(setq ring-bell-function 'ignore)

;; y/n instead of yes/no
(setq use-short-answers t)

;; no scratch message
(setq initial-scratch-message nil)  

(keymap-global-set "M-n" 'scroll-up-line)
(keymap-global-set "M-p" 'scroll-down-line)

(setq-default
 ;; paste at cursor not mouse position
 mouse-yank-at-point t

 ;; saves copies to kill ring 
 save-interprogram-paste-before-kill t 

 ;; frame title
 frame-title-format (format "emacs btw")
 
 ;; Put backup files in  ~/.emacs.d/backups
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))

 ;; Do not autosave.
 auto-save-default nil)

;; Set regex syntax for regex builder tool
;; (setq reb-re-syntax 'string)

;; Move to trash instead of delete
(setq-default delete-by-moving-to-trash t)


;; -------------------- Configure General Packages ----------------------------- 


;; Git client
(use-package magit
  :straight t
  :defer t
  :bind ("C-x g" . magit-status))


;; Garbage Collection Magic Hack
(use-package gcmh
  :straight t
  :defer t
  :config
  (gcmh-mode 1))


(use-package smex
  :straight t
  :defer t
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize)
  :bind ("M-x" . smex))


;; Recent files
(use-package recentf
  :straight
  :bind (:map
	 recentf-mode-map
	 ("C-x C-a" . 'recentf-open-files))
  :config
  (setq
   recentf-auto-cleanup 'never
   recentf-max-saved-items 1000
   recentf-save-file (concat user-emacs-directory ".recentf"))
  (recentf-mode t))


;; Auto COMPlete ANYwhere
(use-package company
  :straight t
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0)
  (global-company-mode t)
  (keymap-global-set "C-c SPC" 'company-complete))


(use-package ido-completing-read+
  :straight t
  :config
  ;; This enables ido in all contexts where it could be useful, not just
  ;; for selecting buffer and file names
  (ido-mode t)
  (ido-everywhere t)
  ;; This allows partial matches, e.g. "uzh" will match "Ustad Zakir Hussain"
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point nil)
  ;; Includes buffer names of recently opened files, even if they're not open now.
  (setq ido-save-directory-list-file (concat user-emacs-directory ".ido.last"))
  (setq ido-use-virtual-buffers t))

;; -------------------- Load Files --------------------


(setq custom-file (concat user-emacs-directory "custom-file.el"))

(load-file custom-file)
(load-file (concat user-emacs-directory "init-window.el"))
(load-file (concat user-emacs-directory "init-programming.el"))
(load-file (concat user-emacs-directory "init-file-manager.el"))


;; -------------------- Look and feel --------------------


(use-package ewal-spacemacs-themes
  :straight t
  :config
  (setq spacemacs-theme-comment-bg nil
        spacemacs-theme-comment-italic nil)
  (load-theme 'spacemacs-dark t))


