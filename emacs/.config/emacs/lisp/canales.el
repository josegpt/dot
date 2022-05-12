;;; canales.el --- Watch HDHomerun channels -*- lexical-binding: t -*-

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

(require 'json)

(defgroup canales ()
  "Watch HDHomerun channels."
  :prefix "canales-"
  :group 'multimedia
  :group 'applications)

(defcustom canales-base-url "http://192.168.1.248"
  "Base url for `canales'"
  :type 'string
  :group 'canales)

(defcustom canales-lineup-url "lineup.json"
  "Lineup url for `canales'"
  :type 'string
  :group 'canales)

(defcustom canales-player-video-command "mpv %s >/dev/null 2>&1"
  "Player command to run when a canal is selected."
  :type 'string
  :group 'canales)

(defun canales--get-channels ()
  "Fetch channels list from url."
  (set-buffer (url-retrieve-synchronously (format "%s/%s" canales-base-url canales-lineup-url)))
  (kill-paragraph 1)
  (delete-char 1)
  (json-parse-buffer :object-type 'plist :array-type 'list))

(defun canales--make-channel (channel)
  "Generate channel from raw data to be processed by the interactive function."
  (let ((drm (plist-get channel :DRM)))
    `(,(format "%4d - %s" (string-to-number (plist-get channel :GuideNumber)) (plist-get channel :GuideName))
      ,(plist-get channel :URL)
      ,(if drm 1 0))))

(defun canales--channel-list ()
  "Generate a list of channels for interactive command."
  (mapcar #'canales--make-channel (canales--get-channels)))

(defun canales--drm-free-p (channel)
  "Predicate for drm-free channels."
  (= (caddr channel) 0))

(defun canales--watch-channel (channel)
  "Watch channel selected."
  (let ((name (car channel))
        (url (cadr channel)))
    (start-process-shell-command "canales-player" nil (format canales-player-video-command url))
    (message "Playing %s" name)))

;;;###autoload
(defun canales-watch ()
  (interactive)
  (let* ((channels (seq-filter #'canales--drm-free-p (canales--channel-list)))
         (choice (completing-read "Canales choose channel: " channels nil t))
         (channel-selected (assoc-string choice channels)))
    (canales--watch-channel channel-selected)))

(provide 'canales)
;;; canales.el ends here
