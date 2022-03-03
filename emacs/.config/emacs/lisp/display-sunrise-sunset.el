;;; display-sunrise-sunset.el --- Display sunrise sunset in the modeline 🌅. -*- lexical-binding: t -*-

;; Copyright (C) 2022 Jose G Perez Taveras <josegpt27@gmail.com>

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

(require 'solar)
(require 'calendar)

(defgroup display-sunrise-sunset ()
  "Display sunrise sunset in the modeline 🌅."
  :prefix "display-sunrise-sunset-"
  :group 'mode-line)

(defcustom display-sunrise-sunset-interval (* 60 60 24)
  "Seconds between updates of sunrise-sunset in the mode line."
  :type 'integer)

(defcustom display-sunrise-sunset-hook nil
  "List of functions to be called when sunrise-sunset is updated in the mode line."
  :type 'hook)

(defvar display-sunrise-sunset-string nil
  "String used in mode line to display sunrise sunset string.
It should not be set directly, but is instead updated by the
`display-sunrise-sunset' function.")
;;;###autoload(put 'display-sunrise-sunset-string 'risky-local-variable t)

(defvar display-sunrise-sunset-timer nil
  "String used in mode line to display sunrise sunset string.
It should not be set directly, but is instead updated by the
`display-sunrise-sunset' function.")
;;;###autoload(put 'display-sunrise-sunset-timer 'risky-local-variable t)

(defun display-sunrise-sunset-update-handler ()
  "Update sunrise-sunset in mode line.
Calcalutes and sets up the timer for the next update of
sunrise-sunset with the specified `display-sunrise-sunset-interval'"
  (display-sunrise-sunset-update)
  (let* ((current (current-time))
         (timer display-sunrise-sunset-timer)
         (next-time (timer-relative-time
                     (list (aref timer 1) (aref timer 2) (aref timer 3))
                     (* 5 (aref timer 4)) 0)))
    (or (time-less-p current next-time)
        (progn
          (timer-set-time timer (timer-next-integral-multiple-of-time
                                 current display-sunrise-sunset-interval)
                          (timer-activate timer))))))

(defun display-sunrise-sunset-update ()
  "Update sunrise-sunset in mode line."
  (let ((calendar-time-display-form '(12-hours ":" minutes am-pm))
        (l (solar-sunrise-sunset (calendar-current-date))))
    (setq display-sunrise-sunset-string
          (format "[↑%s ↓%s] "
                  (apply #'solar-time-string (car l))
                  (apply #'solar-time-string (cadr l))))
    (run-hooks 'display-sunrise-sunset-hook))
  (force-mode-line-update 'all))

;;;###autoload
(defun display-sunrise-sunset ()
  "Enable display of sunrise-sunset in mode line.
This display updates automatically every day.  This runs the
normal hook `display-sunrise-sunset-hook' after each update."
  (interactive)
  (display-sunrise-sunset-mode 1))

;;;###autoload
(define-minor-mode display-sunrise-sunset-mode
  "Toggle display of sunrise-sunset.
When Display Sunrise-Sunset mode is enabled, it updates every
day (you can control the number of seconds between updates by
customizing `display-sunrise-sunset-interval'"
  :global t
  :group 'display-sunrise-sunset
  (and display-sunrise-sunset-timer (cancel-timer display-sunrise-sunset-timer))
  (setq display-sunrise-sunset-string "")
  (when display-sunrise-sunset-mode
    (or 'global-mode-string (setq global-mode-string '("")))
    (or (memq 'display-sunrise-sunset-string global-mode-string)
        (setq global-mode-string
              (append global-mode-string '(display-sunrise-sunset-string))))
    (setq display-sunrise-sunset-timer
          (run-at-time t display-sunrise-sunset-interval
                       'display-sunrise-sunset-update-handler))
    (display-sunrise-sunset-update)))

(provide 'display-sunrise-sunset)
;;; display-sunrise-sunset.el ends here
