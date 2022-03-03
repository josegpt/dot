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

(defvar display-sunrise-sunset-string nil
  "String used in mode line to display sunrise sunset string.
It should not be set directly, but is instead updated by the
`display-sunrise-sunset' function.")
;;;###autoload(put 'display-sunrise-sunset-string 'risky-local-variable t)

(defun display-sunrise-sunset-set ()
  "Set sunrise-sunset in mode line."
  (let ((calendar-time-display-form '(12-hours ":" minutes am-pm))
        (l (solar-sunrise-sunset (calendar-current-date))))
    (setq display-sunrise-sunset-string
          (format "[↑%s ↓%s] "
                  (apply #'solar-time-string (car l))
                  (apply #'solar-time-string (cadr l))))))

;;;###autoload
(defun display-sunrise-sunset ()
  "Enable display of sunrise-sunset in mode line."
  (interactive)
  (display-sunrise-sunset-mode 1))

;;;###autoload
(define-minor-mode display-sunrise-sunset-mode
  "Toggle display of sunrise-sunset."
  :global t
  :group 'display-sunrise-sunset
  (setq display-sunrise-sunset-string "")
  (or 'global-mode-string (setq global-mode-string '("")))
  (or (memq 'display-sunrise-sunset-string global-mode-string)
      (setq global-mode-string
            (append global-mode-string '(display-sunrise-sunset-string))))
  (display-sunrise-sunset-set))

(provide 'display-sunrise-sunset)
;;; display-sunrise-sunset.el ends here
