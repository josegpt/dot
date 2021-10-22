;;; achrome-theme.el --- achrome color theme for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2021-2021 Jose G Perez Taveras

;; Author: Jose G Perez Taveras <josegpt27@gmail.com>
;; URL: http://github.com/josegpt/achrome-theme
;; Version: 0.1

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

(deftheme achrome
  "achrome color theme for Emacs")

;; Please, install rainbow-mode.
(let ((color-accent "#d0021b")
      (color-white  "#ffffff")
      (color-gray+1 "#aaaaaa")
      (color-gray+2 "#888888")
      (color-gray+3 "#5d5d5d")
      (color-gray+4 "#333333")
      (color-gray+5 "#191919")
      (color-black  "#000000")
      (class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'achrome
   ;; Default
   `(border ((,class (:background ,color-gray+5))))
   `(warning ((,class (:foreground ,color-accent))))
   `(which-func ((,class (:foreground ,color-accent))))
   `(match ((,class (:background ,color-accent :foreground ,color-white))))
   `(cursor ((,class (:background ,color-accent :foreground ,color-white))))
   `(default ((,class (:background ,color-black :foreground ,color-gray+1))))

   ;; Sh
   `(sh-heredoc ((,class (:foreground ,color-gray+2))))
   `(sh-quoted-exec ((,class (:foreground ,color-accent :bold t))))

   ;; Minibuffer
   `(minibuffer-prompt ((,class (:foreground ,color-accent :bold t))))

   ;; Highlighting faces
   `(fringe ((,class (:background ,color-gray+5))))
   `(region ((,class (:background ,color-accent :foreground ,color-white))))
   `(highlight ((,class (:background ,color-gray+4 :foreground ,color-white))))
   `(lazy-highlight ((,class (:background ,color-gray+4 :foreground ,color-white))))

   ;; Mode line faces
   `(mode-line-buffer-id ((,class (:foreground ,color-accent :bold t))))
   `(mode-line ((,class (:background ,color-gray+5 :foreground ,color-white))))
   `(mode-line-inactive ((,class (:background ,color-gray+5 :foreground ,color-gray+3))))
   `(mode-line-highlight ((,class (:background ,color-accent :foreground ,color-white))))

   ;; Paren
   `(show-paren-match ((,class (:foreground ,color-accent))))
   `(show-paren-mismatch ((,class (:background ,color-accent :foreground ,color-white))))

   ;; Link
   `(link ((,class (:foreground ,color-gray+2 :underline t))))
   `(link-visited ((,class (:foreground ,color-accent :underline t))))

   ;; Line Number
   `(line-number ((,class (:foreground ,color-gray+3))))
   `(line-number-current-line ((,class (:foreground ,color-accent :bold t))))

   ;; Isearch
   `(isearch-fail ((,class (:foreground ,color-accent))))
   `(isearch ((,class (:background ,color-accent :foreground ,color-white))))

   ;; Which-Key
   `(which-key-key-face ((,class (:foreground ,color-accent))))
   `(which-key-command-description-face ((,class (:foreground ,color-gray+2))))

   ;; Whitespace
   `(whitespace-tab ((,class (:background ,color-black :foreground ,color-gray+4))))
   `(whitespace-line ((,class (:background ,color-black :foreground ,color-gray+4))))
   `(whitespace-space ((,class (:background ,color-black :foreground ,color-gray+4))))
   `(whitespace-empty ((,class (:background ,color-black :foreground ,color-gray+4))))
   `(whitespace-hspace ((,class (:background ,color-black :foreground ,color-gray+4))))
   `(whitespace-newline ((,class (:background ,color-black :foreground ,color-gray+4))))
   `(whitespace-trailing ((,class (:background ,color-gray+4 :foreground ,color-gray+3))))
   `(whitespace-big-indent ((,class (:background ,color-black :foreground ,color-gray+4))))
   `(whitespace-indentation ((,class (:background ,color-black :foreground ,color-gray+4))))
   `(whitespace-space-after-tab ((,class (:background ,color-black :foreground ,color-gray+4))))
   `(whitespace-space-before-tab ((,class (:background ,color-black :foreground ,color-gray+4))))

   ;; Font lock faces
   `(font-lock-string-face ((,class (:foreground ,color-gray+1))))
   `(font-lock-builtin-face ((,class (:foreground ,color-accent))))
   `(font-lock-comment-face ((,class (:foreground ,color-gray+3))))
   `(font-lock-warning-face ((,class (:foreground ,color-accent))))
   `(font-lock-constant-face ((,class (:foreground ,color-accent))))
   `(font-lock-type-face ((,class (:foreground ,color-accent :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,color-accent :weight bold))))
   `(font-lock-variable-name-face ((,class (:foreground ,color-gray+3 :bold t))))
   `(font-lock-function-name-face ((,class (:foreground ,color-accent :bold t))))
))

(provide-theme 'achrome)

;;; achrome-theme.el ends here
