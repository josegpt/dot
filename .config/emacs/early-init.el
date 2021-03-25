;;; disable package.el at startup
(setq package-enable-at-startup nil)

;;; set alias to y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; configure font size
(set-face-attribute 'default nil :family "Iosevka" :height 135)

;; matching paren
(show-paren-mode t)

;; disable tool bar
(tool-bar-mode -1)

;; more space
(set-fringe-mode 4)

;; disbale tooltip
(tooltip-mode -1)

;; disable scroll bars
(scroll-bar-mode -1)
(toggle-scroll-bar -1)

;;;; ===> Utils <===
;;; check if linux is running
(defvar is-linux-p
  (string= system-type "gnu/linux")
  "detect if linux is running")

;;; duplicate line
(defun duplicate-line ()
  "Duplicate current line"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank))

;;; apply settings base of hostname
(defun if-pc (name fn &optional args) "call function per system base"
       (when (string-equal system-name name)
         (apply fn args)))

(defun check-hostname (name)
  (string= system-name name))
