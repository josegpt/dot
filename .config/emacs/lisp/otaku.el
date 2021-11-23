;;; otaku.el --- Search and watch anime from Emacs -*- lexical-binding: t -*-

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

;; Folder structure
;; . otaku
;; |-- search-that time
;  `-- anime-tensei-shitara-slime-datta-ken-2nd-season

;;; Code:

(eval-when-compile
  (require 'json))

(defgroup otaku ()
  "Search and watch anime from Emacs."
  :prefix "otaku-"
  :group 'multimedia
  :group 'applications)

(defcustom otaku-base-url "https://gogoanime.cm"
  "Base url."
  :type 'string
  :group 'otaku)

(defcustom otaku-mpv-video-command "mpv --http-header-fields=\"Referer: %s\" %s >/dev/null"
  "Mpv command to run when a video is found."
  :type 'string
  :group 'otaku)

(defcustom otaku-older-than (* 60 60 24)
  "When otaku considers a file old enough to be replaced.")

(defvar otaku-last-search nil
  "Store last search")

;;; Marginalia

(defun otaku-annotate-episode (cand)
  "Marginalia annotator for `otaku'"
  (let* ((slug (cadr otaku-last-search))
         (anime (otaku--repository-get-anime slug))
         (title (gethash "title" anime))
         (status (gethash "status" anime))
         (type (gethash "type" anime))
         (released-date (gethash "released-date" anime))
         (last-episode (gethash "last-episode" anime))
         (summary (gethash "summary" anime)))
    (concat (propertize " " 'display '(space :align-to center))
            (propertize title 'face '(italic font-lock-keyword-face))
            " "
            (propertize (format "%10d" last-episode) 'face '(bold font-lock-constant-face))
            " "
            (propertize (format "%10s" released-date) 'face '(font-lock-comment-face))
            " "
            (propertize (format "%10s" status) 'face '(font-lock-string-face))
            " "
            (propertize (format "%10s" type) 'face '(font-lock-builtin-face)))))

(with-eval-after-load 'marginalia
  (add-to-list 'marginalia-annotator-registry
               '(otaku-anime otaku-annotate-episode builtin none))
  (add-to-list 'marginalia-prompt-categories '("\\<[Cc]hoose .* [Ee]pisode\\>" . otaku-anime)))

;;; Helpers

(defun otaku--list-from-hash (key1 key2 hash)
  "List from hash"
  (let ((result '()))
    (dolist (obj hash)
      (push `(,(gethash key1 obj) ,(gethash key2 obj)) result))
    result))

(defun otaku-search-url (keyword)
  "Search url constructor."
  (format "%s/search.html?keyword=%s" otaku-base-url (url-encode-url keyword)))

(defun otaku-single-anime-url (path)
  "Single anime url constructor."
  (format "%s/category/%s" otaku-base-url (url-encode-url path)))

(defun otaku-anime-episode-url (path episode)
  "Anime episode url constructor."
  (format "%s/%s-episode-%s" otaku-base-url path episode))

(defun otaku-mpv-command (referer video-url)
  "Mpv command constructor"
  (format otaku-mpv-video-command referer video-url))

;; CRUD

(defcustom otaku-db-dir (format "%s/.cache/%s" (getenv "HOME") "otaku")
  "Directory where all searched animes are store for better performance."
  :type 'string
  :group 'otaku)

(defun otaku--db-insert (file-path object)
  "DB insert."
  (unless (file-exists-p file-path)
    (with-temp-buffer
      (json-insert object)
      (write-file file-path nil))))

(defun otaku--db-exists-p (file-path)
  "DB exists predicate."
  (info-file-exists-p file-path))

(defun otaku--db-select (file-path)
  "DB select."
  (when (file-exists-p file-path)
    (let* ((json-object-type 'hash-table)
           (json-array-type 'list)
           (json-key-type 'string))
          (json-read-file file-path))))

(defun otaku--db-update (file-path new-object)
  "DB update."
  (otaku--db-delete file-path)
  (otaku--db-insert file-path new-object))

(defun otaku--db-delete (file-path)
  "DB delete."
  (when (file-exists-p file-path)
    (delete-file file-path)))

(defun otaku--db-is-old-p (file-path)
  "DB is older predicate."
  (let* ((time (time-convert (current-time) 'integer))
        (file-time (time-convert
                    (file-attribute-modification-time
                     (file-attributes file-path 'string)) 'integer))
        (difference (- time file-time)))
    (> difference otaku-older-than)))

(defun otaku--db-handler (prefix action slug &optional object)
  "DB handler."
  (unless (file-directory-p otaku-db-dir)
    (make-directory otaku-db-dir))
  (let ((file-path (format "%s/%s-%s" otaku-db-dir prefix slug)))
    (cond
     ((string= action "insert") (otaku--db-insert file-path object))
     ((string= action "select") (otaku--db-select file-path))
     ((string= action "update") (otaku--db-update file-path object))
     ((string= action "delete") (otaku--db-delete file-path))
     ((string= action "exists") (otaku--db-exists-p file-path))
     ((string= action "older") (otaku--db-is-old-p file-path))
     (t (error "unknown action: %s" action)))))

;;; Repository

;; Anime

(defun otaku--repository-insert-anime (slug anime)
  "Insert anime."
  (otaku--db-handler "anime" "insert" slug anime))

(defun otaku--repository-exists-anime-p (slug)
  "Check anime."
  (otaku--db-handler "anime" "exists" slug))

(defun otaku--repository-is-old-anime-p (slug)
  "Check if anime is old."
  (otaku--db-handler "anime" "older" slug))

(defun otaku--repository-get-anime (slug)
  "Get anime."
  (otaku--db-handler "anime" "select" slug))

(defun otaku--repository-update-anime (slug anime)
  "Update anime."
  (otaku--db-handler "anime" "update" slug anime))

(defun otaku--repository-delete-anime (slug)
  "Delete anime."
  (otaku--db-handler "anime" "delete" slug))

;; Search

(defun otaku--repository-insert-search (keyword search)
  "Insert search."
  (otaku--db-handler "search" "insert" keyword search))

(defun otaku--repository-exists-search-p (keyword)
  "Check search."
  (otaku--db-handler "search" "exists" keyword))

(defun otaku--repository-is-old-search-p (keyword)
  "Check if search is old."
  (otaku--db-handler "search" "older" keyword))

(defun otaku--repository-get-search (keyword)
  "Get search."
  (otaku--db-handler "search" "select" keyword))

(defun otaku--repository-update-search (keyword search)
  "Update search."
  (otaku--db-handler "search" "update" keyword search))

(defun otaku--repository-delete-search (keyword)
  "Delete search."
  (otaku--db-handler "search" "delete" keyword))

;;; HTTP

(defun otaku--http-search-anime-by-keyword (keyword)
  "Search anime by keyword."
  (let ((animes (list)))
    (set-buffer (url-retrieve-synchronously (otaku-search-url keyword)))
    (while (re-search-forward "<a href=\"/category/\\(.+\\)\" title=\"\\(.+\\)\".+</a>" nil t)
      (push `((title . ,(match-string 2))
              (slug . ,(match-string 1))) animes))
    `((keyword . ,keyword)
      (result . [,@animes]))))

(defun otaku--http-get-single-anime (slug)
  "Get single anime by slug."
  (let* ((buffer (url-retrieve-synchronously (otaku-single-anime-url slug)))
         (title (otaku--search-anime-title buffer))
         (type (otaku--search-anime-type buffer))
         (summary (otaku--search-anime-summary buffer))
         ;; FIXME: find a way to parse it correctly
         (released-date (otaku--search-anime-released-date buffer))
         (status (otaku--search-anime-status buffer))
         (episodes-range (otaku--search-anime-episodes-range buffer))
         (last-episode (cadr episodes-range))
         (episodes (otaku--episodes-list slug last-episode)))
    `((title . ,title)
      (slug . ,slug)
      (type . ,type)
      (summary . ,summary)
      (released-date . ,released-date)
      (status . ,status)
      (last-episode . ,last-episode)
      (episodes . [,@episodes]))))

(defun otaku--episodes-list (slug end)
  "Construct episodes path"
  (let ((result '()))
    (dotimes (episode end)
      (push `((title . ,(format "Episode %d" (1+ episode)))
              (episode . ,(1+ episode))
              (slug . ,(otaku-anime-episode-url slug (1+ episode)))) result))
    result))

(defun otaku--search-anime-title (buffer)
  "Search anime title"
  (set-buffer buffer)
  (re-search-forward "<h1.*>\\(.*\\)</h1>" nil t)
  (match-string 1))

(defun otaku--search-anime-type (buffer)
  "Search anime type"
  (set-buffer buffer)
  (re-search-forward "<a href=\"/sub-category/.*\">\\(.*\\)</a>" nil t)
  (match-string 1))

(defun otaku--search-anime-summary (buffer)
  "Search anime summary"
  (set-buffer buffer)
  (re-search-forward "<p class=\"type.+><span>Plot Summary.*</span>\\(.*\\)" nil t)
  (match-string 1))

;; FIXME: Does not get all genres
(defun otaku--search-anime-genres (buffer)
  "Search anime genres"
  (set-buffer buffer)
  (re-search-forward "<a href=\"https\:.*/genre/.*\" title=\"\\(action\\|adventure\\)\">\\(action\\|adventure\\)</a>" nil t)
  (match-string 1))

(defun otaku--search-anime-released-date (buffer)
  "Search anime released date"
  (set-buffer buffer)
  (re-search-forward "<p class=\"type.+>Released.*</span>\\(.*\\)</p>" nil t)
  (match-string 1))

(defun otaku--search-anime-status (buffer)
  "Search anime status"
  (set-buffer buffer)
  (re-search-forward "<a href=\"/\\(completed\\|ongoing\\).*>\\(.*\\)</a>" nil t)
  (match-string 2))

(defun otaku--search-anime-episodes-range (buffer)
  "Search anime episodes range."
  (let ((ranges '()))
    (set-buffer buffer)
    (while (re-search-forward "<a href=.+ class=.+ ep_start = '\\([0-9]*\\)' ep_end = '\\([0-9]*\\)'>.+</a>" nil t)
      (setq ranges (append `(,(string-to-number (match-string 1)) ,(string-to-number (match-string 2))) ranges)))
    `(,(apply #'min ranges) ,(apply #'max ranges))))

(defun otaku--search-anime-episode-video-url (url)
  "Search episode video url for selected anime."
  (set-buffer (url-retrieve-synchronously url))
  (re-search-forward "<a href=\"#\" rel=\"100\" data-video=\"\\(.+\\)\" >.+</a>" nil t)
  (format "https:%s" (match-string 1)))

(defun otaku--search-anime-episode-inner-video-url (url)
  "Search inner episode video url to be played in mpv."
  (set-buffer (url-retrieve-synchronously url))
  (re-search-forward "sources:\\[{file: '\\(.+\\)',label.+" nil t)
  (match-string 1))

(defun otaku--service-search-anime-by-keyword (keyword)
  "Search anime by keyword."
  (cond
   ((not (otaku--repository-exists-search-p keyword))
    (otaku--repository-insert-search keyword (otaku--http-search-anime-by-keyword keyword)))
   ((otaku--repository-is-old-search-p keyword)
    (otaku--repository-update-search keyword (otaku--http-search-anime-by-keyword keyword))))
  (otaku--repository-get-search keyword))

(defun otaku--service-get-single-anime (slug)
  "Get anime by slug"
  (cond
   ((not (otaku--repository-exists-anime-p slug))
    (otaku--repository-insert-anime slug (otaku--http-get-single-anime slug)))
   ((otaku--repository-is-old-anime-p slug)
    (otaku--repository-update-anime slug (otaku--http-get-single-anime slug))))
  (otaku--repository-get-anime slug))

(defun otaku-watch-episode (episode)
  "Watch selected episode."
  (let* ((slug (cadr episode))
         (referer (otaku--search-anime-episode-video-url slug))
         (video-url (otaku--search-anime-episode-inner-video-url referer)))
    (start-process-shell-command "otaku-mpv" nil (otaku-mpv-command referer video-url))
    (message "%s sent to mpv" slug)))

(defun otaku-select-episode (anime)
  (let* ((title (car anime))
         (slug (cadr anime))
         (episodes (gethash "episodes" (otaku--service-get-single-anime slug)))
         (episodes-list (otaku--list-from-hash "title" "slug" episodes))
         (choice (completing-read (format "Choose %s Episode: " title) episodes-list nil t))
         (episode-selected (assoc-string choice episodes-list)))
    (otaku-watch-episode episode-selected)))

;;; Interactive

;;;###autoload
(defun otaku-search-anime (keyword)
  "Search anime by keyword."
  (interactive "sOtaku Search Anime: ")
  (if (string= keyword "")
      (if otaku-last-search
          (otaku-select-episode otaku-last-search)
        (error "No anime has been searched."))
    (let* ((result (otaku--service-search-anime-by-keyword keyword))
           (animes (otaku--list-from-hash "title" "slug" (gethash "result" result)))
           (choice (completing-read "Choose Anime: " animes nil t))
           (anime-selected (assoc-string choice animes)))
      (setq otaku-last-search anime-selected)
      (otaku-select-episode anime-selected))))

(provide 'otaku)
;;; otaku.el ends here
