;;; achrome-theme.el --- ahcrome color theme for Emacs.

;; Copyright (C) 2021 Jose G Perez Taveras a.k.a josegpt

;; Author: Jose G Perez Taveras <josegpt27@gmail.com>
;; Version: 1.0.0

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;;; Code:

(deftheme achrome
  "achrome is a monochrome theme with a accent color.")

(let ((class '((class color) (min-colors 89)))
      (accent "#d0021b")
      (gray-0 "#c4c4c4")
      (gray-1 "#ababab")
      (gray-2 "#787878")
      (gray-3 "#454545")
      (gray-4 "#0a0a0a")
      (gray-5 "#111111"))
  (custom-theme-set-faces
   'achrome

   ;; Defaults
   `(error ((,class (:foreground, accent))))
   `(cursor ((,class (:foreground, gray-5 :background, accent))))
   `(default ((,class (:foreground, gray-2 :background, gray-5))))

   ;; Whitespace
   `(whitespace-space ((,class (:foreground, gray-4 :background, gray-5))))

   ;; Line number
   `(line-number ((,class (:foreground, gray-3))))
   `(line-number-current-line ((,class (:foreground, accent :weight bold))))

   ;; Modeline
   `(mode-line ((,class (:foreground, gray-1 :background, gray-4))))
   `(mode-line-inactive ((,class (:foreground, gray-3 :background, gray-4))))

   ;; Font lock faces
   `(font-lock-string-face ((,class (:foreground, gray-0))))
   `(font-lock-comment-face ((,class (:foreground, gray-3))))
   `(font-lock-builtin-face ((,class (:foreground, gray-3))))
   `(font-lock-constant-face ((,class (:foreground, gray-3))))
   `(font-lock-variable-name-face ((,class (:foreground, gray-3))))
   `(font-lock-function-name-face ((,class (:foreground, gray-3))))
   `(font-lock-type-face ((,class (:foreground, gray-2 :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground, accent :weight bold))))

   ))


(provide-theme 'achrome)

;;; achrome-theme.el ends here
