;;; scrcpy.el --- Emacs scrcpy wrapper                     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Joshua Carrasco

;; Author: Joshua Carrasco <mail@joshuacarrasco.com>
;; Keywords: hardware

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; An Emacs wrapper for scrcpy, a command line utility to control Android devices
;; connected via USB or TCP/IP.
;;
;; Steps on Android:
;; - Plug device into computer with USB
;; - In developer options
;;   - Enable "USB debugging"
;;   - (optional) Enable "Disable adb authorization timeout"
;;   - (optional) In developer options change "Default USB Configuration" to "File Transfer / Android Auto"
;;     Unless you want to pick on manually select this option everytime.

;;; Code:

(defgroup scrcpy nil
  "Scrcpy integration for Emacs."
  :prefix "scrcpy-" :group 'tools)

(defcustom scrcpy-devices-buffer "*scrcpy devices*"
  "Connected devices list buffer name."
  :group 'scrcpy
  :type 'string)

(defcustom scrcpy-scrcpy-command "scrcpy"
  "Command to use for scrcpy."
  :group 'scrcpy
  :type 'string)

(defcustom scrcpy-adb-command "adb"
  "Command to use for Android Bridge."
  :group 'scrcpy
  :type 'string)

(defcustom scrcpy-network-addr "192.168.2.15:5555"
  "Default network address that adb will listen on for TCP."
  :group 'scrcpy
  :type 'string)

(defcustom scrcpy-command-arguments
  '(("--video-codec" . "h265")
    ("--audio-buffer" . "200")
    ("--max-size" . "1920")
    ("--max-fps" . "60")
    ("--keyboard" . "uhid")
    ("--audio-codec" . "aac"))
  "Default command line arguments to pass to scrcpy."
  :group 'scrcpy
  :type 'alist)

(defun scrcpy-adb-connect ()
  "Connect to default host and port."
  (interactive)
  (message (shell-command-to-string (concat scrcpy-adb-command " connect " (format-network-address scrcpy-network-addr)))))

(defun scrcpy-adb-disconnect ()
  "Connect to HOST[:PORT] on adb."
  (interactive)
  (message (shell-command-to-string (concat scrcpy-adb-command " disconnect " (format-network-address scrcpy-network-addr)))))

(defun scrcpy-open ()
  "Open a connection."
  (interactive)
  (switch-to-buffer "*scrcpy*")
  (shell-command (concat scrcpy-scrcpy-command
                         " "
                         (mapconcat
                          (lambda (cell)
                            (concat (car cell) "=" (cdr cell)))
                          scrcpy-command-arguments
                          " ")
                         " &")
                 (current-buffer))
  (read-only-mode)
  (scrcpy-mode))

(defvar scrcpy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'kill-buffer-and-window)
    (define-key map (kbd "c") #'scrcpy-adb-connect)
    (define-key map (kbd "d") #'scrcpy-adb-disconnect)
    (define-key map (kbd "s") #'scrcpy-open)
    map)
  "Keymap for scrcpy mode commands.")

(define-minor-mode scrcpy-mode
  "Scrcpy integration for Emacs."
  :keymap scrcpy-mode-map
  :group 'scrcpy
  :require 'scrcpy)

(provide 'scrcpy)
;;; scrcpy.el ends here
