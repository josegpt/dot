;;   ___ _ __ ___   __ _  ___ ___    ___  __ _ _ __| |_   _
;;  / _ \ '_ ` _ \ / _` |/ __/ __|  / _ \/ _` | '__| | | | |
;; |  __/ | | | | | (_| | (__\__ \ |  __/ (_| | |  | | |_| |
;;  \___|_| |_| |_|\__,_|\___|___/  \___|\__,_|_|  |_|\__, |
;;                                                    |___/
;; ============================================================
;;; disable package.el at startup
(setq package-enable-at-startup nil)

;;; set alias to y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;;; configure font size
(set-face-attribute 'default nil :family "Iosevka" :height 120)

;;; matching paren
(show-paren-mode t)

;;; highlight current line
(global-hl-line-mode t)

;;; disable tool bar
(tool-bar-mode 0)

;;; more space
(set-fringe-mode 8)

;;; disbale tooltip
(tooltip-mode 0)

;;; disable scroll bars
(scroll-bar-mode 0)
(toggle-scroll-bar 0)

;; ============================================================
;; Functions
;; ============================================================
(defun duplicate-line ()
  "Duplicate current line"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank))

(defun check-hostname (name)
  "Check hostname of pc"
  (string= system-name name))

(defvar powersettings-menu
  '(("Reboot" . "doas reboot")
    ("Poweroff" . "doas poweroff"))
  "define options for powersettings fn")

(defun powersettings ()
  "execute powersettings base on selection"
  (interactive)
  (let* ((completion-ignore t)
         (item (assoc-string
                (completing-read "Power Settings: " powersettings-menu  nil t)
                powersettings-menu t))
         (cmd (cdr item)))
    (shell-command cmd)))
