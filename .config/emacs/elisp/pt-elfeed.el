;;; pt-elfeed.el --- Extra functionality to elfeed rss reader -*- lexical-binding: t -*-

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

(defgroup pt-elfeed ()
  "Extra functionality to elfeed rss reader."
  :group 'comm)

;;; Helpers

(defun pt-elfeed--valid-youtube-link (entry)
  (let ((link (elfeed-entry-link entry)))
    (when (string-match-p "^\\(https?\:\/\/\\)?\\(\\(www\.\\|m\.\\)?youtube\.com\\|youtu\.be\\)" link)
      link)))

(defun pt-elfeed--mpv-command (link)
  (call-process "mpv" nil 0 nil link))

;;; Interactive

;;;###autoload
(defun pt-elfeed-play-youtube-link ()
  "Play the selected feed youtube link with mpv."
  (interactive)
  (let* ((entries (elfeed-search-selected))
         (links (remq nil (mapcar #'pt-elfeed--valid-youtube-link entries)))
         (links-str (mapconcat #'identity links " ")))
    (when entries
      (elfeed-untag entries 'unread)
      (message "%d" (length links))
      (if (> (length links) 0)
          (progn
            (mapc #'pt-elfeed--mpv-command links)
            (message "Playing: %s" links-str))
        (message "No videos to play"))
      (mapc #'elfeed-search-update-entry entries)
      (unless (or elfeed-search-remain-on-entry (use-region-p))
        (forward-line)))))


(provide 'pt-elfeed)
;;; pt-elfeed.el ends here
