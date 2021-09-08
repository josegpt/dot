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

(defcustom pt-desktop-set-audio-volume-increment "pactl -- set-sink-volume 0 +10%"
  "Command to increment audio volume"
  :type 'string
  :group 'pt-desktop)

(defcustom pt-desktop-set-audio-volume-decrement "pactl -- set-sink-volume 0 -10%"
  "Command to decrement audio volume"
  :type 'string
  :group 'pt-desktop)

(defcustom pt-desktop-set-audio-mute "pactl -- set-sink-mute 0 toggle"
  "Command to toggle audio mute"
  :type 'string
  :group 'pt-desktop)

(defcustom pt-desktop-get-audio-volume "pactl -- get-sink-volume 0"
  "Command to get audio volume"
  :type 'string
  :group 'pt-desktop)

(defcustom pt-desktop-get-audio-mute "pactl -- get-sink-mute 0"
  "Command to get audio mute status"
  :type 'string
  :group 'pt-desktop)

;; Audio Mic

(defcustom pt-desktop-set-audio-mic-volume-increment "pactl -- set-source-volume 1 +10%"
  "Command to increment audio mic volume"
  :type 'string
  :group 'pt-desktop)

(defcustom pt-desktop-set-audio-mic-volume-decrement "pactl -- set-source-volume 1 -10%"
  "Command to decrement audio mic volume"
  :type 'string
  :group 'pt-desktop)

(defcustom pt-desktop-set-audio-mic-mute "pactl -- set-source-mute 1 toggle"
  "Command to toggle mic mute"
  :type 'string
  :group 'pt-desktop)

(defcustom pt-desktop-get-audio-mic-volume "pactl -- get-source-volume 1"
  "Command to get audio mic volume"
  :type 'string
  :group 'pt-desktop)

(defcustom pt-desktop-get-audio-mic-mute "pactl -- get-source-mute 1"
  "Command to get audio mic volume"
  :type 'string
  :group 'pt-desktop)

;; Customization -- Brightness

(defcustom pt-desktop-set-brightness-increment "xbacklight -inc 10%"
  "Command to increment brightness of screen"
  :type 'string
  :group 'pt-desktop)

(defcustom pt-desktop-set-brightness-decrement "xbacklight -dec 10%"
  "Command to decrement brightness of screen"
  :type 'string
  :group 'pt-desktop)

(defcustom pt-desktop-get-brightness "xbacklight -get"
  "Command to lower brightness of screen"
  :type 'string
  :group 'pt-desktop)

(defun pt-desktop--format-brightness-message (val)
  "Format brightness message to a more readable format"
  (let ((n (string-to-number val)))
    (message "Brightness %d%%" (round n))))

;; Helper functions

(defun pt-desktop--run-command (cmd)
  "Run commands pt-desktop commands"
  (shell-command-to-string cmd))

;; Interactive functions

;;;###autoload
(defun pt-desktop-audio-volume-increment ()
  "Interactive function to increment audio volume"
  (interactive)
  (pt-desktop--run-command pt-desktop-set-audio-volume-increment)
  (message "Audio %s" (pt-desktop--run-command pt-desktop-get-audio-volume)))

;;;###autoload
(defun pt-desktop-audio-volume-decrement ()
  "Interactive function to decrement audio volume"
  (interactive)
  (pt-desktop--run-command pt-desktop-set-audio-volume-decrement)
  (message "Audio %s" (pt-desktop--run-command pt-desktop-get-audio-volume)))

;;;###autoload
(defun pt-desktop-audio-mute-toggle ()
  "Interactive function to mute audio volume"
  (interactive)
  (pt-desktop--run-command pt-desktop-set-audio-mute)
  (message "Audio %s" (pt-desktop--run-command pt-desktop-get-audio-mute)))

;;;###autoload
(defun pt-desktop-audio-mic-volume-increment ()
  "Interactive function to increment audio volume"
  (interactive)
  (pt-desktop--run-command pt-desktop-set-audio-mic-volume-increment)
  (message "Audio Mic %s" (pt-desktop--run-command pt-desktop-get-audio-mic-volume)))

;;;###autoload
(defun pt-desktop-audio-mic-volume-decrement ()
  "Interactive function to decrement audio mic volume"
  (interactive)
  (pt-desktop--run-command pt-desktop-set-audio-mic-volume-decrement)
  (message "Audio Mic %s" (pt-desktop--run-command pt-desktop-get-audio-mic-volume)))

;;;###autoload
(defun pt-desktop-audio-mic-mute-toggle ()
  "Interactive function to mute audio mic volume"
  (interactive)
  (pt-desktop--run-command pt-desktop-set-audio-mic-mute)
  (message "Audio Mic %s" (pt-desktop--run-command pt-desktop-get-audio-mic-mute)))

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

(provide 'pt-desktop)
;;; pt-desktop.el ends here
