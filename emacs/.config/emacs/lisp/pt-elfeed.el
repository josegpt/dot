;;; pt-elfeed.el --- extra funtionality to elfeed.el -*- lexical-binding: t -*-

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

(defgroup pt-elfeed ()
  "extra funtionality to elfeed.el."
  :prefix "pt-elfeed-"
  :group 'group)

(defun pt-elfeed-start-mpv (video-url)
  (start-process-shell-command
   "elfeed-mpv" nil (format "mpv %s > /dev/null 2>&1" video-url)))

(defun pt-elfeed-mpv-watch ()
  "Watch the selected feed items in mpv."
  (interactive)
  (let* ((entries (elfeed-search-selected))
         (yt-entries (seq-filter (apply-partially #'elfeed-tagged-p 'yt) entries))
         (titles (mapcar #'elfeed-entry-title yt-entries))
         (links (mapcar #'elfeed-entry-link yt-entries))
         (titles-str (mapconcat #'identity titles " | ")))
    (when yt-entries
      (elfeed-untag yt-entries 'unread)
      (mapc #'pt-elfeed-start-mpv links)
      (message "Watch: %s" titles-str)
      (mapc #'elfeed-search-update-entry yt-entries)
      (unless (or elfeed-search-remain-on-entry (use-region-p))
        (forward-line)))))

(provide 'pt-elfeed)
;;; pt-elfeed.el ends here
