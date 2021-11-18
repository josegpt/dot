;;; weeb.el --- Search and watch anime from Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jose G Perez Taveras <josegpt27@gmail.com>
;; Version: 0.9

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

;; Folder structure
;; . weeb
;; |-- search-that time
;  `-- anime-tensei-shitara-slime-datta-ken-2nd-season

;;; Code:

(require 'json)

(defgroup weeb ()
  "Search and watch anime from Emacs."
  :prefix "weeb-"
  :group 'multimedia
  :group 'applications)

(defcustom weeb-base-url "https://gogoanime.cm"
  "Base url."
  :type 'string
  :group 'weeb)

(defcustom weeb-mpv-video-command "mpv --http-header-fields=\"Referer: %s\" %s >/dev/null"
  "Mpv command to run when a video is found."
  :type 'string
  :group 'weeb)

(defcustom weeb-older-than (* 60 60 24)
  "When weeb considers a file old enough to be replaced.")

;;; Helpers

;; TODO: Make it async
(defmacro scrape-from-uri (uri &rest body)
  "Macro to scrape from uri"
  `(let ((ssl-opt (if (string= (substring ,uri 0 5) "https") "-k" "")))
     (with-temp-buffer
       (call-process "curl" nil t nil "-s" "-A" "Mozilla/5.0" ssl-opt ,uri)
       (goto-char (point-min))
       ,@body)))

(defun weeb--list-from-hash (key1 key2 hash)
  "List from hash"
  (let ((result '()))
    (dolist (obj hash)
      (push `(,(gethash key1 obj) ,(gethash key2 obj)) result))
    result))

(defun weeb--anime-field-accessor (field anime)
  "Anime field accessor."
  (cdr (assoc-string field anime)))

(defun weeb-search-url (keyword)
  "Search url constructor."
  (format "%s/search.html?keyword=%s" weeb-base-url (url-encode-url keyword)))

(defun weeb-single-anime-url (path)
  "Single anime url constructor."
  (format "%s/category/%s" weeb-base-url (url-encode-url path)))

(defun weeb-anime-episode-url (path episode)
  "Anime episode url constructor."
  (format "%s/%s-episode-%s" weeb-base-url path episode))

(defun weeb-mpv-command (referer video-url)
  "Mpv command constructor"
  (format weeb-mpv-video-command referer video-url))

;; CRUD

(defcustom weeb-db-dir (format "%s/.cache/%s" (getenv "HOME") "weeb")
  "Directory where all searched animes are store for better performance."
  :type 'string
  :group 'weeb)

(defun weeb--db-insert (file-path object)
  "DB insert."
  (unless (file-exists-p file-path)
    (with-temp-buffer
      (json-insert object)
      (write-file file-path nil))))

(defun weeb--db-exists-p (file-path)
  "DB exists predicate."
  (info-file-exists-p file-path))

(defun weeb--db-select (file-path)
  "DB select."
  (when (file-exists-p file-path)
    (let* ((json-object-type 'hash-table)
           (json-array-type 'list)
           (json-key-type 'string))
          (json-read-file file-path))))

(defun weeb--db-update (file-path new-object)
  "DB update."
  (weeb--db-delete file-path)
  (weeb--db-insert file-path new-object))

(defun weeb--db-delete (file-path)
  "DB delete."
  (when (file-exists-p file-path)
    (delete-file file-path)))

(defun weeb--db-is-old-p (file-path)
  "DB is older predicate."
  (let* ((time (time-convert (current-time) 'integer))
        (file-time (time-convert
                    (file-attribute-modification-time
                     (file-attributes file-path 'string)) 'integer))
        (difference (- time file-time)))
    (> difference weeb-older-than)))

(defun weeb--db-handler (prefix action slug &optional object)
  "DB handler."
  (unless (file-directory-p weeb-db-dir)
    (make-directory weeb-db-dir))
  (let ((file-path (format "%s/%s-%s" weeb-db-dir prefix slug)))
    (cond
     ((string= action "insert") (weeb--db-insert file-path object))
     ((string= action "select") (weeb--db-select file-path))
     ((string= action "update") (weeb--db-update file-path object))
     ((string= action "delete") (weeb--db-delete file-path))
     ((string= action "exists") (weeb--db-exists-p file-path))
     ((string= action "older") (weeb--db-is-old-p file-path))
     (t (error "unknown action: %s" action)))))

;;; Repository

;; Anime

(defun weeb--repository-insert-anime (slug anime)
  "Insert anime."
  (weeb--db-handler "anime" "insert" slug anime))

(defun weeb--repository-exists-anime-p (slug)
  "Check anime."
  (weeb--db-handler "anime" "exists" slug))

(defun weeb--repository-is-old-anime-p (slug)
  "Check if anime is old."
  (weeb--db-handler "anime" "older" slug))

(defun weeb--repository-get-anime (slug)
  "Get anime."
  (weeb--db-handler "anime" "select" slug))

(defun weeb--repository-update-anime (slug anime)
  "Update anime."
  (weeb--db-handler "anime" "update" slug anime))

(defun weeb--repository-delete-anime (slug)
  "Delete anime."
  (weeb--db-handler "anime" "delete" slug))

;; Search

(defun weeb--repository-insert-search (keyword search)
  "Insert search."
  (weeb--db-handler "search" "insert" keyword search))

(defun weeb--repository-exists-search-p (keyword)
  "Check search."
  (weeb--db-handler "search" "exists" keyword))

(defun weeb--repository-is-old-search-p (keyword)
  "Check if search is old."
  (weeb--db-handler "search" "older" keyword))

(defun weeb--repository-get-search (keyword)
  "Get search."
  (weeb--db-handler "search" "select" keyword))

(defun weeb--repository-update-search (keyword search)
  "Update search."
  (weeb--db-handler "search" "update" keyword search))

(defun weeb--repository-delete-search (keyword)
  "Delete search."
  (weeb--db-handler "search" "delete" keyword))

;;; HTTP

(defun weeb--http-search-anime-by-keyword (keyword)
  "Search anime by keyword."
  (let ((animes (list)))
    (set-buffer (url-retrieve-synchronously (weeb-search-url keyword)))
    (while (re-search-forward "<a href=\"/category/\\(.+\\)\" title=\"\\(.+\\)\".+</a>" nil t)
      (push `((title . ,(match-string 2))
              (slug . ,(match-string 1))) animes))
    `((keyword . ,keyword)
      (result . [,@animes]))))

(defun weeb--http-get-single-anime (slug)
  "Get single anime by slug."
  (let* ((buffer (url-retrieve-synchronously (weeb-single-anime-url slug)))
         (title (weeb--search-anime-title buffer))
         (type (weeb--search-anime-type buffer))
         (summary (weeb--search-anime-summary buffer))
         ;; FIXME: find a way to parse it correctly
         (released-date (weeb--search-anime-released-date buffer))
         (status (weeb--search-anime-status buffer))
         (episodes-range (weeb--search-anime-episodes-range buffer))
         (last-episode (cadr episodes-range))
         (episodes (weeb--episodes-list slug last-episode)))
    `((title . ,title)
      (slug . ,slug)
      (type . ,type)
      (summary . ,summary)
      (released-date . ,released-date)
      (status . ,status)
      (last-episode . ,last-episode)
      (episodes . [,@episodes]))))

(defun weeb--episodes-list (slug end)
  "Construct episodes path"
  (let ((result '()))
    (dotimes (episode end)
      (push `((title . ,(format "Episode %d" (1+ episode)))
              (episode . ,(1+ episode))
              (slug . ,(weeb-anime-episode-url slug (1+ episode)))) result))
    result))

(defun weeb--search-anime-title (buffer)
  "Search anime title"
  (set-buffer buffer)
  (re-search-forward weeb-anime-title-regex nil t)
  (match-string 1))

(defun weeb--search-anime-type (buffer)
  "Search anime type"
  (set-buffer buffer)
  (re-search-forward weeb-anime-type-regex nil t)
  (match-string 1))

(defun weeb--search-anime-summary (buffer)
  "Search anime summary"
  (set-buffer buffer)
  (re-search-forward "<p class=\"type.+><span>Plot Summary.*</span>\\(.*\\)" nil t)
  (match-string 1))

;; FIXME: Does not get all genres
(defun weeb--search-anime-genres (buffer)
  "Search anime genres"
  (set-buffer buffer)
  (re-search-forward "<a href=\"https\:.*/genre/.*\" title=\"\\(action\\|adventure\\)\">\\(action\\|adventure\\)</a>" nil t)
  (match-string 1))

(defun weeb--search-anime-released-date (buffer)
  "Search anime released date"
  (set-buffer buffer)
  (re-search-forward "<p class=\"type.+>Released.*</span>\\(.*\\)</p>" nil t)
  (match-string 1))

(defun weeb--search-anime-status (buffer)
  "Search anime status"
  (set-buffer buffer)
  (re-search-forward "<a href=\"/\\(completed\\|ongoing\\).*>\\(.*\\)</a>" nil t)
  (match-string 2))

(defun weeb--search-anime-episodes-range (buffer)
  "Search anime episodes range."
  (let ((ranges '()))
    (set-buffer buffer)
    (while (re-search-forward "<a href=.+ class=.+ ep_start = '\\([0-9]*\\)' ep_end = '\\([0-9]*\\)'>.+</a>" nil t)
      (setq ranges (append `(,(string-to-number (match-string 1)) ,(string-to-number (match-string 2))) ranges)))
    `(,(apply #'min ranges) ,(apply #'max ranges))))

(defun weeb--search-anime-episode-video-url (url)
  "Search episode video url for selected anime."
  (set-buffer (url-retrieve-synchronously url))
  (re-search-forward "<a href=\"#\" rel=\"100\" data-video=\"\\(.+\\)\" >.+</a>" nil t)
  (format "https:%s" (match-string 1)))

(defun weeb--search-anime-episode-inner-video-url (url)
  "Search inner episode video url to be played in mpv."
  (set-buffer (url-retrieve-synchronously url))
  (re-search-forward "sources:\\[{file: '\\(.+\\)',label.+" nil t)
  (match-string 1))

;;; Service

(defun weeb--service-search-anime-by-keyword (keyword)
  "Search anime by keyword."
  (cond
   ((not (weeb--repository-exists-search-p keyword))
    (weeb--repository-insert-search keyword (weeb--http-search-anime-by-keyword keyword)))
   ((weeb--repository-is-old-search-p keyword)
    (weeb--repository-update-search keyword (weeb--http-search-anime-by-keyword keyword))))
  (weeb--repository-get-search keyword))

(defun weeb--service-get-single-anime (slug)
  "Get anime by slug"
  (cond
   ((not (weeb--repository-exists-anime-p slug)) (weeb--repository-insert-anime slug (weeb--http-get-single-anime slug)))
   ((weeb--repository-is-old-anime-p slug) (weeb--repository-update-anime slug (weeb--http-get-single-anime slug))))
  (weeb--repository-get-anime slug))

(defun weeb-watch-episode (episode)
  "Watch selected episode."
  (let* ((slug (cadr episode))
         (referer (weeb--search-anime-episode-video-url slug))
         (video-url (weeb--search-anime-episode-inner-video-url referer)))
    (start-process-shell-command "weeb-mpv" nil (weeb-mpv-command referer video-url))
    (message "%s sent to mpv" slug)))

(defun weeb-select-episode (anime)
  (let* ((title (car anime))
         (slug (cadr anime))
         (episodes (gethash "episodes" (weeb--service-get-single-anime slug)))
         (episodes-list (weeb--list-from-hash "title" "slug" episodes))
         (choice (completing-read (format "Choose %s Episode: " title) episodes-list nil t))
         (episode-selected (assoc-string choice episodes-list)))
    (weeb-watch-episode episode-selected)))
  
;;;###autoload
(defun weeb-search-anime (keyword)
  "Search anime by keyword."
  (interactive "sWeeb Search Anime: ")
  (let* ((result (weeb--service-search-anime-by-keyword keyword))
         (animes (weeb--list-from-hash "title" "slug" (gethash "result" result)))
         (choice (completing-read "Choose Anime: " animes nil t))
         (anime-selected (assoc-string choice animes)))
    (weeb-select-episode anime-selected)))

(provide 'weeb)
;;; weeb.el ends here
