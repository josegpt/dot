;;; pt-desktop.el --- Desktop extras for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jose G Perez Taveras <josegpt27@gmail.com>

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;;; Code:

(defgroup pt-desktop ()
  "Functions to control desktop environment."
  :group 'environment)

;; Customization -- Volume

;; Audio

(defcustom pt-desktop-set-audio-volume-increment "amixer set Master 10%+"
  "Command to increment audio volume"
  :type 'string
  :group 'pt-desktop)

(defcustom pt-desktop-set-audio-volume-decrement "amixer set Master 10%-"
  "Command to decrement audio volume"
  :type 'string
  :group 'pt-desktop)

(defcustom pt-desktop-set-audio-mute "amixer set Master toggle"
  "Command to toggle audio mute"
  :type 'string
  :group 'pt-desktop)

(defcustom pt-desktop-get-audio-volume-status "amixer get Master"
  "Command to get audio volume"
  :type 'string
  :group 'pt-desktop)

;; Audio Mic

(defcustom pt-desktop-set-audio-mic-volume-increment "amixer set Capture 10%+"
  "Command to increment audio mic volume"
  :type 'string
  :group 'pt-desktop)

(defcustom pt-desktop-set-audio-mic-volume-decrement "amixer set Capture 10%-"
  "Command to decrement audio mic volume"
  :type 'string
  :group 'pt-desktop)

(defcustom pt-desktop-set-audio-mic-mute "amixer set Capture toggle"
  "Command to toggle mic mute"
  :type 'string
  :group 'pt-desktop)

(defcustom pt-desktop-get-audio-mic-volume-status "amixer get Capture"
  "Command to get audio mic volume"
  :type 'string
  :group 'pt-desktop)

;; Customization -- Brightness

(defcustom pt-desktop-set-brightness-increment "light -A 10"
  "Command to increment brightness of screen"
  :type 'string
  :group 'pt-desktop)

(defcustom pt-desktop-set-brightness-decrement "light -U 10"
  "Command to decrement brightness of screen"
  :type 'string
  :group 'pt-desktop)

(defcustom pt-desktop-get-brightness "light -G"
  "Command to lower brightness of screen"
  :type 'string
  :group 'pt-desktop)

(defcustom pt-desktop-powersetting-commands '(("Reboot" . "sudo reboot")
                                              ("Poweroff" . "sudo shutdown"))
  "Commands to control power settings of the computer"
  :type 'list
  :group 'pt-desktop)

(defun pt-desktop--format-brightness-message (val)
  "Format brightness message to a more readable format"
  (let ((n (string-to-number val)))
    (message "Brightness %d%%" (round n))))

;; Helper functions

(defun pt-desktop--run-command (cmd)
  "Run commands pt-desktop commands"
  (shell-command-to-string cmd))

(defun pt-desktop--func-helper (cmmd1 cmmd2 msg)
  "Run commands pt-desktop commands"
  (shell-command cmmd1)
  (message "%s %s" msg (shell-command-to-string cmmd2)))

;; Interactive functions

;;;###autoload
(defun pt-desktop-audio-volume-increment ()
  "Interactive function to increment audio volume"
  (interactive)
  (pt-desktop--func-helper
   pt-desktop-set-audio-volume-increment
   pt-desktop-get-audio-volume-status "Audio"))

;;;###autoload
(defun pt-desktop-audio-volume-decrement ()
  "Interactive function to decrement audio volume"
  (interactive)
  (pt-desktop--func-helper
   pt-desktop-set-audio-volume-decrement
   pt-desktop-get-audio-volume-status "Audio"))

;;;###autoload
(defun pt-desktop-audio-mute-toggle ()
  "Interactive function to mute audio volume"
  (interactive)
  (pt-desktop--func-helper
   pt-desktop-set-audio-mute
   pt-desktop-get-audio-volume-status "Audio"))

;;;###autoload
(defun pt-desktop-audio-mic-volume-increment ()
  "Interactive function to increment audio mic volume"
  (interactive)
  (pt-desktop--func-helper
   pt-desktop-set-audio-mic-volume-increment
   pt-desktop-get-audio-mic-volume-status "Audio Mic"))

;;;###autoload
(defun pt-desktop-audio-mic-volume-decrement ()
  "Interactive function to decrement audio mic volume"
  (interactive)
  (pt-desktop--func-helper
   pt-desktop-set-audio-mic-volume-decrement
   pt-desktop-get-audio-mic-volume-status "Audio Mic"))

;;;###autoload
(defun pt-desktop-audio-mic-mute-toggle ()
  "Interactive function to mute audio mic volume"
  (interactive)
  (pt-desktop--func-helper
   pt-desktop-set-audio-mic-mute
   pt-desktop-get-audio-mic-volume-status "Audio Mic"))

;;;###autoload
(defun pt-desktop-brightness-increment ()
  "Interactive function to increment brightness"
  (interactive)
  (pt-desktop--run-command pt-desktop-set-brightness-increment)
  (pt-desktop--format-brightness-message (pt-desktop--run-command pt-desktop-get-brightness)))

;;;###autoload
(defun pt-desktop-brightness-decrement ()
  "Interactive function to decrement brightness"
  (interactive)
  (pt-desktop--run-command pt-desktop-set-brightness-decrement)
  (pt-desktop--format-brightness-message (pt-desktop--run-command pt-desktop-get-brightness)))

;;;###autoload
(defun pt-desktop-powersettings ()
  (interactive)
  (let* ((completion-ignore t)
         (choice (assoc-string
                  (completing-read "Action: " pt-desktop-powersetting-commands  nil t)
                  pt-desktop-powersetting-commands t))
         (cmd (cdr choice)))
    (eshell-command cmd)))

(provide 'pt-desktop)
;;; pt-desktop.el ends here
