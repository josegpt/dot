;;; weeb.el --- Search and watch anime from Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jose G Perez Taveras <josegpt27@gmail.com>
;; Version: 0.1

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

(defgroup weeb ()
  "Search and watch anime from Emacs."
  :prefix "weeb-"
  :group 'multimedia
  :group 'applications)

(defcustom weeb-base-url "https://gogoanime.pe"
  "Weeb base url"
  :type 'string
  :group 'weeb)

(defun weeb--fetch-anime-list (name)
  (let ((animes (list)))
    (set-buffer (url-retrieve-synchronously (format "%s/search.html?keyword=%s" weeb-base-url (url-encode-url name))))
    (while (re-search-forward "<a href=\"\/category/\\(.+\\)\" title=\"\\(.+\\)\".+</a>" nil t)
      (push `(,(match-string 2) . ,(match-string 1)) animes))
    (let ((choice (assoc-string
                   (completing-read "Choose Anime: " animes nil t)
                   animes t)))
      (weeb--fetch-anime-episodes (car choice) (cdr choice)))))

(defun weeb--fetch-anime-episodes (anime path)
  (set-buffer (url-retrieve-synchronously (format "%s/category/%s" weeb-base-url (url-encode-url path))))
  (re-search-forward "<a href=.+ class=.+ ep_start = '\\([0-9]*\\)' ep_end = '\\([0-9]*\\)'>.+</a>" nil t)
  (let* ((first-episode (1+ (string-to-number (match-string 1))))
         (last-episode (string-to-number (match-string 2)))
         (choice (read-number (format "Choose %s Episode[%d-%d]: " anime first-episode last-episode))))
    (if (and (>= choice first-episode) (<= choice last-episode))
        (weeb--fetch-anime-video-url anime path choice)
      (message "Input out of range."))))

(defun weeb--fetch-anime-video-url (anime path episode-number)
  (set-buffer (url-retrieve-synchronously (format "%s/%s-episode-%s" weeb-base-url path episode-number)))
  (re-search-forward "<a href=\"#\" rel=\"100\" data-video=\"\\(.+\\)\" >.+</a>" nil t)
  (weeb--fetch-anime-inner-video-url anime episode-number (format "https:%s" (match-string 1))))

(defun weeb--fetch-anime-inner-video-url (anime episode-number url)
  (set-buffer (url-retrieve-synchronously url))
  (re-search-forward "sources:\\[{file: '\\(.+\\)',label.+" nil t)
  (start-process-shell-command "mpv" nil (format "mpv --http-header-fields=\"Referer: %s\" %s >/dev/null" url (match-string 1)))
  (message "%s - Episode %s sent to mpv" anime episode-number))

(defun weeb-search-anime (name)
  (interactive "sWeeb Search Anime: ")
  (weeb--fetch-anime-list name))

;; (fetch-anime-list "that time")

;; (fetch-anime-episodes "That Time" "tensei-shitara-slime-datta-ken")

;; (fetch-anime-video-url "https://gogoanime.pe/tensei-shitara-slime-datta-ken-episode-1")

;; (fetch-anime-inner-video-url "https://gogoplay1.com/embedplus?id=MTEwMzY5&token=5zLoTU45an_b7UK48ehtoA&expires=1636086857")

;; mpv command
;; mpv --http-header-fields="Referer: https://gogoplay1.com/embedplus?id=MTEwMzY5&token=5zLoTU45an_b7UK48ehtoA&expires=1636086857" https://www06.anicdn.stream/videos/hls/UMn7vGSdJNI3z1D7iH43dw/1636087458/110369/f14491b150096bfd3899a59cab0f1cb8/ep.1.1604877580.m3u8

;; embedded
;; https://gogoplay1.com/embedplus?id=MTEwMzY5&token=5zLoTU45an_b7UK48ehtoA&expires=1636086857

;; video
;; https://www06.anicdn.stream/videos/hls/UMn7vGSdJNI3z1D7iH43dw/1636087458/110369/f14491b150096bfd3899a59cab0f1cb8/ep.1.1604877580.m3u8

;; Animes
;; "<a href=\"\/category/\\(.+\\)\" title=\"\\(.+\\)\".+</a>"
;; Episodes
;; "<a href=.+ class=.+ ep_start = '\\([0-9]*\\)' ep_end = '\\([0-9]*\\)'>.+</a>"
;; Video
;; "<a href=\"#\" rel=\"100\" data-video=\"\/\/\\(.+\\)\" >.+</a>"
;; Inner video
;; "sources:\\[{file: '\\(.+\\)',label.+"

;; (defun fetch-anime ()
;;   (interactive)
;;   (let ((temp-buf-name "*animes*"))
;;     (get-buffer-create temp-buf-name)
;;     (set-buffer temp-buf-name)
;;     (url-insert-file-contents "https://gogoanime.pe/tensei-shitara-slime-datta-ken-episode-1")
;;     ;; (insert (url-retrieve-synchronously "https://josegpt.com"))
;;     (switch-to-buffer temp-buf-name)
;;     ))

(provide 'weeb)
;;; weeb.el ends here
