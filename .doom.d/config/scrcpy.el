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

;;; Code:
(defcustom scrcpy-connections-alist
;; adb connect DEVICE_IP:5555 ;;
  '(
    ("phone1" . "scrcpy --video-codec=h265 --audio-buffer=200 --max-size=1920 --max-fps=60 --keyboard=uhid --audio-codec=aac &"))

    
  "An alist to map connection name to scrcpy shell command."
  :type 'alist
  :group 'scrcpy)

(defun scrcpy-open ()
  "Open a connection."
  (interactive)
  (if (length= scrcpy-connections-alist 1)
      (let ((cmd (cdr (nth 0 scrcpy-connections-alist))))
           (shell-command cmd))
    (shell-command "ls")))
  

(provide 'scrcpy)
;;; scrcpy.el ends here
