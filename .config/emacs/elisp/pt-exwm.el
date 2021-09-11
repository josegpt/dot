;;; pt-exwm.el --- Functions enhacing exwm workspace for my dotfiles -*- lexical-binding: t -*-

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

(require 'exwm-core)

(defgroup pt-exwm ()
  "Functions enhacing exwm workspace for my dotfiles."
  :group 'exwm)

(defun pt-exwm-rename-buffer-with-class-name ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun pt-exwm-custom-rename-buffer-with-title ()
  (pcase exwm-class-name
    ("Chromium-browser" (exwm-workspace-rename-buffer (format "Chromium: %s" exwm-title)))))

(defun pt-exwm-send-window-to-workspace ()
  (pcase exwm-class-name
    ("Chromium-browser" (exwm-workspace-move-window 4))))

(defun pt-exwm-run-app (command)
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))

(defun pt-exwm-xrandr-config ()
  (start-process-shell-command
   "xrandr" nil "xrandr --setmonitor HDMI-1-1 1080/286x2160/572+1380+0 HDMI-1")
  (start-process-shell-command
   "xrandr" nil "xrandr --setmonitor HDMI-1-2 1380/368x1080/286+0+1080 none")
  (start-process-shell-command
   "xrandr" nil "xrandr --setmonitor HDMI-1-3 1380/368x1080/286+0+0 none")
  (start-process-shell-command
   "xrandr" nil "xrandr --setmonitor HDMI-1-4 1380/368x1080/286+2460+0 none")
  (start-process-shell-command
   "xrandr" nil "xrandr --setmonitor HDMI-1-5 1380/368x1080/286+2460+1080 none"))

(provide 'pt-exwm)
;;; pt-exwm.el ends here
