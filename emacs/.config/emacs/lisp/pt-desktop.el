;;; pt-desktop.el --- Funtionality for the X environment -*- lexical-binding: t -*-

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

(require 'exwm-workspace)

(defgroup pt-desktop ()
  "Funtionality for the X environment."
  :prefix "pt-desktop-"
  :group 'application)

(defcustom pt-desktop-powersetting-commands '(("Reboot" . "rb")
                                              ("Poweroff" . "po"))
  "Powersetting commands"
  :type 'list
  :group 'application)

;;;###autoload
(defun pt-desktop--run-command-with-message (cmmd)
  (start-process-shell-command "pt-desktop" nil cmmd))

;;;###autoload
(defun pt-desktop-rename-workspace-buffer ()
  (exwm-workspace-rename-buffer exwm-title))

;;;###autoload
(defun pt-desktop-play-pause-player ()
  (interactive)
  (pt-desktop--run-command-with-message "playerctl play-pause"))

;;;###autoload
(defun pt-desktop-stop-player ()
  (interactive)
  (pt-desktop--run-command-with-message "playerctl stop"))

;;;###autoload
(defun pt-desktop-next-player ()
  (interactive)
  (pt-desktop--run-command-with-message "playerctl next"))

;;;###autoload
(defun pt-desktop-previous-player ()
  (interactive)
  (pt-desktop--run-command-with-message "playerctl previous"))

;;;###autoload
(defun pt-desktop-raise-volume ()
  (interactive)
  (pt-desktop--run-command-with-message "amixer set Master 10%+"))

;;;###autoload
(defun pt-desktop-lower-volume ()
  (interactive)
  (pt-desktop--run-command-with-message "amixer set Master 10%-"))

;;;###autoload
(defun pt-desktop-mute-volume ()
  (interactive)
  (pt-desktop--run-command-with-message "amixer set Master toggle"))

;;;###autoload
(defun pt-desktop-raise-mic-volume ()
  (interactive)
  (pt-desktop--run-command-with-message "amixer set Capture 10%+"))

;;;###autoload
(defun pt-desktop-lower-mic-volume ()
  (interactive)
  (pt-desktop--run-command-with-message "amixer set Capture 10%-"))

;;;###autoload
(defun pt-desktop-mute-mic-volume ()
  (interactive)
  (pt-desktop--run-command-with-message "amixer set Capture toggle"))

;;;###autoload
(defun pt-desktop-raise-brightness ()
  (interactive)
  (pt-desktop--run-command-with-message "xbacklight -inc 10%"))

;;;###autoload
(defun pt-desktop-lower-brightness ()
  (interactive)
  (pt-desktop--run-command-with-message "xbacklight -dec 10%"))

;;;###autoload
(defun pt-desktop-powersettings ()
  (interactive)
  (let* ((choice (assoc-string
                  (completing-read "Powersettings Action: " pt-desktop-powersetting-commands  nil t)
                  pt-desktop-powersetting-commands t))
         (cmmd (cdr choice)))
    (eshell-command cmmd)))

;;;###autoload
(defun pt-desktop-previous-workspace ()
  (interactive)
  (if (< 0 exwm-workspace-current-index)
      (exwm-workspace-switch (1- exwm-workspace-current-index))
    (exwm-workspace-switch (1- (exwm-workspace--count)))))

;;;###autoload
(defun pt-desktop-next-workspace ()
  (interactive)
  (if (> (1- (exwm-workspace--count)) exwm-workspace-current-index)
      (exwm-workspace-switch (1+ exwm-workspace-current-index))
    (exwm-workspace-switch 0)))

(provide 'pt-desktop)
;;; pt-desktop.el ends here
