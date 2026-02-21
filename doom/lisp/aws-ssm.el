;;; aws-ssm.el --- AWS SSM Session Manager helpers          -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Joshua Carrasco
;;
;; Author: Joshua Carrasco <mail@joshuacarrasco.com>
;; Keywords: tools, convenience, aws
;; Package-Requires: ((emacs "27.1"))
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; aws-ssm.el provides a small Emacs wrapper around:
;;
;;   aws ssm start-session
;;
;; The main workflow is:
;; - Store one or more SSM session configs in ~/.authinfo (or ~/.authinfo.gpg)
;;   using a machine name prefix (default: "emacs-aws-ssm/").
;; - Run `aws-ssm-start-session` to pick a config via completing-read.
;; - Emacs opens a dedicated buffer named: *aws-ssm-NAME*
;; - Killing that buffer terminates the underlying SSM session process.
;;
;; Authinfo format (recommended):
;;
;;   machine emacs-aws-ssm/name-here login config \
;;     password "(:target \"i-abc...\" :profile \"prod\" :region \"us-east-1\" :parameters \"host=...,localPortNumber=...,portNumber=...\")"
;;
;; Notes:
;; - The config is stored as an Elisp plist string in the password/secret field.

;;; Code:

(require 'auth-source)
(require 'comint)
(require 'subr-x)
(require 'seq)
(require 'json)

(defgroup aws-ssm nil
  "AWS SSM helpers."
  :group 'tools)

(defcustom aws-ssm-machine-prefix "emacs-aws-ssm/"
  "Prefix of authinfo machine names that represent SSM configs."
  :type 'string
  :group 'aws-ssm)

(defcustom aws-ssm-buffer-prefix "*aws-ssm-"
  "Prefix for SSM session buffers."
  :type 'string
  :group 'aws-ssm)

(defun aws-ssm--kill-process-on-buffer-kill ()
  "Kill the buffer's process (if any) when the buffer is killed."
  (let ((proc (get-buffer-process (current-buffer))))
    (when (and proc (process-live-p proc))
      ;; SIGTERM is usually enough; AWS CLI cleans up the session.
      (ignore-errors (kill-process proc)))))

(defun aws-ssm--entry-name (machine)
  "Extract NAME from MACHINE given `aws-ssm-machine-prefix`."
  (string-remove-prefix aws-ssm-machine-prefix machine))

(defun aws-ssm--read-secret-config (entry)
  "Return a plist config from auth-source ENTRY's secret/password.
Expects minified JSON object (no spaces) in the password/secret."
  (let* ((secret (plist-get entry :secret))
         (raw (cond
               ((functionp secret) (funcall secret))
               ((stringp secret) secret)
               (t nil))))
    (unless (and raw (stringp raw) (not (string-empty-p raw)))
      (user-error "Auth entry has no usable secret/password"))
    (setq raw (string-trim raw))
    (unless (string-prefix-p "{" raw)
      (user-error "Expected JSON object in secret/password, got: %S" raw))
    (let* ((obj (json-parse-string raw :object-type 'alist :array-type 'list))
           (get (lambda (k) (alist-get k obj nil nil #'string=))))
      (list :target     (funcall get "target")
            :profile    (funcall get "profile")
            :region     (funcall get "region")
            :parameters (funcall get "parameters")))))

(defun aws-ssm--get-configs ()
  "Return a list of (DISPLAY-NAME . AUTH-ENTRY) for matching authinfo entries."
  (let* ((candidates (auth-source-search :max 200 :require '(:host :secret)))
         (filtered
          (seq-filter
           (lambda (e)
             (let ((host (plist-get e :host)))
               (and (stringp host)
                    (string-prefix-p aws-ssm-machine-prefix host))))
           candidates)))
    (unless filtered
      (user-error "No authinfo entries found with machine prefix %S"
                  aws-ssm-machine-prefix))
    (mapcar
     (lambda (e)
       (let* ((machine (plist-get e :host))
              (name (aws-ssm--entry-name machine)))
         (cons name e)))
     filtered)))

(defun aws-ssm--start-session (name cfg)
  "Start an SSM start-session process in a comint buffer named by NAME using CFG plist."
  (let* ((target (plist-get cfg :target))
         (profile (plist-get cfg :profile))
         (region (plist-get cfg :region))
         (parameters (plist-get cfg :parameters))
         (bufname (format "%s%s*" aws-ssm-buffer-prefix name))
         (buf (get-buffer-create bufname))
         ;; Build argv to avoid shell quoting issues.
         (argv (list "aws" "ssm" "start-session"
                     "--target" target
                     "--document-name" "AWS-StartPortForwardingSessionToRemoteHost"
                     "--parameters" parameters
                     "--profile" profile
                     "--region" region)))

    (dolist (pair `((:target . ,target)
                    (:profile . ,profile)
                    (:region . ,region)
                    (:parameters . ,parameters)))
      (unless (cdr pair)
        (user-error "Missing %S in config %S" (car pair) name)))

    ;; Keep 1 process per buffer (restarting same NAME replaces the old proc).
    (when-let ((old (get-buffer-process buf)))
      (when (process-live-p old)
        (ignore-errors (kill-process old))))

    (with-current-buffer buf
      (erase-buffer)
      (comint-mode)
      (let ((cmd (mapconcat #'shell-quote-argument argv " ")))
        (insert "$ " cmd "\n\n"))
      (setq-local buffer-read-only t)
      (read-only-mode 1)
      (setq-local truncate-lines t)
      (when (featurep 'evil)
        (evil-normal-state))

      (let ((proc (apply #'start-process (format "aws-ssm-%s" name) buf argv)))
        (set-process-query-on-exit-flag proc nil))

      (add-hook 'kill-buffer-hook #'aws-ssm--kill-process-on-buffer-kill nil t))

    (pop-to-buffer buf)))

;;;###autoload
(defun aws-ssm-start-session ()
  "Pick an SSM config from authinfo and start it in its own buffer. Killing the buffer terminates the session."
  (interactive)
  (let* ((configs (aws-ssm--get-configs))
         (choice (completing-read "AWS SSM config: " (mapcar #'car configs) nil t))
         (entry (cdr (assoc choice configs)))
         (cfg (aws-ssm--read-secret-config entry)))
    (aws-ssm--start-session choice cfg)))

(provide 'aws-ssm)
;;; aws-ssm.el ends here
