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

;; Folder structure
;; . weeb
;; `-- tensei-shitara-slime-datta-ken-2nd-season

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

(defcustom weeb-db-dir (format "%s/.cache/%s" (getenv "HOME") "weeb")
  "Directory where all searched animes are store for better performance."
  :type 'string
  :group 'weeb)

(defcustom weeb-mpv-video-command "mpv --http-header-fields=\"Referer: %s\" %s >/dev/null"
  "Mpv command to run when a video is found."
  :type 'string
  :group 'weeb)

;;; Helpers

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

(defun weeb-selected ()
  "Find selected entry"
  (let* ((episodes-total (length weeb-anime-episodes))
         (episode-number (- (line-number-at-pos (point)) weeb-anime-offset))
         (within-limits (and (>= episode-number 0) (< episode-number episodes-total))))
    (when (and within-limits (arrayp weeb-anime-episodes))
      (aref weeb-anime-episodes episode-number))))

(defvar weeb-anime-episodes nil
  "Anime episodes")

(defvar weeb-anime-offset 9
  "Offset list")

(defvar weeb-anime-card-regex "<a href=\"/category/\\(.+\\)\" title=\"\\(.+\\)\".+</a>"
  "Regex to find all animes.")

(defvar weeb-anime-title-regex "<h1.*>\\(.*\\)</h1>"
  "Regex to find anime title.")

(defvar weeb-anime-type-regex "<a href=\"/sub-category/.*\">\\(.*\\)</a>"
  "Regex to find anime type.")

(defvar weeb-anime-summary-regex "<p class=\"type.+><span>Plot Summary.*</span>\\(.*\\)"
  "Regex to find anime summary.")

;; TODO: Find a better way to identify each genre
(defvar weeb-anime-genre-regex "<a href=\"https\:.*/genre/.*\" title=\"\\(action\\|adventure\\)\">\\(action\\|adventure\\)</a>"
  "Regex to find anime genre")

(defvar weeb-anime-released-date-regex "<p class=\"type.+>Released.*</span>\\(.*\\)</p>"
  "Regex to find anime released date.")

(defvar weeb-anime-status-regex "<a href=\"/\\(completed\\|ongoing\\).*>\\(.*\\)</a>"
  "Regex to find anime status")

(defvar weeb-anime-episodes-range-regex "<a href=.+ class=.+ ep_start = '\\([0-9]*\\)' ep_end = '\\([0-9]*\\)'>.+</a>"
  "Regex to find anime episodes range.")

(defvar weeb-anime-video-url-regex "<a href=\"#\" rel=\"100\" data-video=\"\\(.+\\)\" >.+</a>"
  "Regex to find the video url for the selected anime.")

(defvar weeb-inner-episode-video-url-regex "sources:\\[{file: '\\(.+\\)',label.+"
  "Inner episode video regex.")

;;; DB

(defun weeb--db-file-setter (file-path slug object)
  "DB file setter"
  (with-temp-buffer
    (json-insert object)
    (write-file file-path nil)))

(defun weeb--db-file-getter (file-path)
  "DB file getter"
  (json-read-file file-path))

(defun weeb--db-handler (slug)
  "DB file handler"
  (unless (file-directory-p weeb-db-dir)
    (make-directory weeb-db-dir))
  (let ((file-path (format "%s/%s" weeb-db-dir slug)))
    (unless (file-exists-p file-path)
      (weeb--db-file-setter file-path slug (weeb--search-anime-by-slug slug)))
    (weeb--db-file-getter file-path)))

;;; HTTP

(defun weeb--search-anime-by-keyword (keyword)
  "Search anime by keyword."
  (let ((animes (list)))
    (set-buffer (url-retrieve-synchronously (weeb-search-url keyword)))
    (while (re-search-forward weeb-anime-card-regex nil t)
      (push `(,(match-string 2) . ,(match-string 1)) animes))
    animes))

(defun weeb--search-anime-by-slug (slug)
  "Fetch single anime by slug."
  (let* ((buffer (url-retrieve-synchronously (weeb-single-anime-url slug)))
         (title (weeb--search-anime-title buffer))
         (type (weeb--search-anime-type buffer))
         (summary (weeb--search-anime-summary buffer))
         ;; FIXME: find a way to parse it correctly
         (genres (weeb--search-anime-genres buffer))
         (released-date (weeb--search-anime-released-date buffer))
         (status (weeb--search-anime-status buffer))
         (episodes-range (weeb--search-anime-episodes-range buffer))
         (first-episode (1+ (car episodes-range)))
         (last-episode (cadr episodes-range))
         (episodes (weeb--episodes-list slug first-episode last-episode)))
    `((title . ,title)
      (slug . ,slug)
      (type . ,type)
      (summary . ,summary)
      (released-date . ,released-date)
      (status . ,status)
      (first-episode . ,first-episode)
      (last-episode . ,last-episode)
      (episodes . [,@episodes]))))

(defun weeb--episodes-list (slug start end)
  "Construct episodes path"
  (mapcar (apply-partially #'weeb-anime-episode-url slug) (number-sequence start end)))

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
  (re-search-forward weeb-anime-summary-regex nil t)
  (match-string 1))

(defun weeb--search-anime-genres (buffer)
  "Search anime genres"
  (set-buffer buffer)
  (re-search-forward weeb-anime-genre-regex nil t)
  (match-string 1))

(defun weeb--search-anime-released-date (buffer)
  "Search anime released date"
  (set-buffer buffer)
  (re-search-forward weeb-anime-released-date-regex nil t)
  (match-string 1))

(defun weeb--search-anime-status (buffer)
  "Search anime status"
  (set-buffer buffer)
  (re-search-forward weeb-anime-status-regex nil t)
  (match-string 2))

(defun weeb--search-anime-episodes-range (buffer)
  "Search anime episodes range."
  (let ((ranges '()))
    (set-buffer buffer)
    (while (re-search-forward weeb-anime-episodes-range-regex nil t)
      (setq ranges (append `(,(string-to-number (match-string 1)) ,(string-to-number (match-string 2))) ranges)))
    `(,(apply #'min ranges) ,(apply #'max ranges))))

(defun weeb--search-anime-episode-video-url (url)
  "Search episode video url for selected anime."
  (set-buffer (url-retrieve-synchronously url))
  (re-search-forward weeb-anime-video-url-regex nil t)
  (format "https:%s" (match-string 1)))

(defun weeb--search-anime-episode-inner-video-url (url)
  "Search inner episode video url to be played in mpv."
  (set-buffer (url-retrieve-synchronously url))
  (re-search-forward weeb-inner-episode-video-url-regex nil t)
  (match-string 1))

;;; Format

(defun weeb--anime-title-format (text)
  ""
  (insert (format "Title: %s\n" text)))

(defun weeb--anime-type-format (text)
  ""
  (insert (format "Type: %s\n" text)))

(defun weeb--anime-episodes-format (text)
  ""
  (insert (format "Episodes: %d\n" text)))

(defun weeb--anime-released-date-format (text)
  ""
  (insert (format "Released Date: %s\n" text)))

(defun weeb--anime-status-format (text)
  ""
  (insert (format "Status: %s\n" text)))

(defun weeb--anime-summary-format (text)
  ""
  (insert (format "Summary: %s\n" text)))

(defun weeb--anime-genres-format (text)
  ""
  (insert (format "Genres: %s\n" text)))

(defun weeb--anime-slug-format (text)
  ""
  (insert (format "Slug: %s\n" text)))

(defun weeb--anime-episode-format (episode)
  "Episode list format."
  (insert (propertize (format "Episode %d\n" episode) 'face 'keyword)))

(defun weeb--anime-episode-entries (start end)
  "Episode list generator"
  (mapcan #'weeb--anime-episode-format (number-sequence start end)))

(defun weeb--mode-view (anime)
  "Render `weeb-mode' since it is not attached to a extension"
  (let* ((title (weeb--anime-field-accessor "title" anime))
         (status (weeb--anime-field-accessor "status" anime))
         (type (weeb--anime-field-accessor "type" anime))
         (released-date (weeb--anime-field-accessor "released-date" anime))
         (summary (weeb--anime-field-accessor "summary" anime))
         (slug (weeb--anime-field-accessor "slug" anime))
         (first-episode (weeb--anime-field-accessor "first-episode" anime))
         (last-episode (weeb--anime-field-accessor "last-episode" anime))
         (episodes (weeb--anime-field-accessor "episodes" anime))
         (buffer (get-buffer-create (format "*Weeb: %s*" title))))
    (with-current-buffer buffer
      (weeb--anime-title-format title)
      (weeb--anime-status-format status)
      (weeb--anime-type-format type)
      ;; (weeb--anime-genres-format genres)
      (weeb--anime-released-date-format released-date)
      (weeb--anime-slug-format slug)
      (weeb--anime-episodes-format last-episode)
      (weeb--anime-summary-format summary)
      (newline)
      (weeb--anime-episode-entries first-episode last-episode)
      (goto-char (point-min))
      (weeb-mode)
      (setf weeb-anime-episodes episodes)
      (switch-to-buffer buffer))))

;;; Mode

(defun weeb-watch-episode (episode)
  "Watch the currently selected entry."
  (interactive (list (weeb-selected)))
  (when episode
    (let* ((referer (weeb--search-anime-episode-video-url episode))
           (video-url (weeb--search-anime-episode-inner-video-url referer)))
      (start-process-shell-command "weeb-mpv" nil (weeb-mpv-command referer video-url)))))

(defun weeb-search-anime (keyword)
  "Search anime by keyword."
  (interactive "sWeeb Search Anime: ")
  (if (string= keyword "")
      (message "TODO: fetching latest episodes not implemented")
    (let* ((animes (weeb--search-anime-by-keyword keyword))
           (anime-selected (assoc-string (completing-read "Choose Anime: " animes nil t)
                                         animes t))
           (slug (cdr anime-selected))
           (anime (weeb--db-handler slug)))
      (weeb--mode-view anime))))

(defvar weeb-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "Weeb")))
    (define-key map (kbd "C-p") #'previous-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "C-n") #'next-line)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "s") #'weeb-search-anime)
    (define-key map (kbd "RET") #'weeb-watch-episode)
    (define-key map (kbd "w") #'weeb-watch-episode)
    map)
  "Keymap for `weeb-mode'.")

(defvar weeb-font-lock-keywords
  '(("Title\\|Episodes\\|Status\\|Type\\|Genres\\|Summary\\|Released Date\\|Slug" . 'font-lock-constant-face)
    ("Episode" . 'font-lock-keyword-face))
  "Keyword highlighting specification for `weeb-mode'.")

(defvar weeb-mode-hook nil
  "Hook for `weeb-mode'.")

;;;###autoload
(define-derived-mode weeb-mode special-mode "Weeb"
  "Definition for `weeb-mode'."
  (make-variable-buffer-local 'weeb-anime-episodes)
  (setq-local font-lock-defaults '(weeb-font-lock-keywords)))

(provide 'weeb)
;;; weeb.el ends here
