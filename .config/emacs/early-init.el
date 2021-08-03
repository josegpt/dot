;;   ___ _ __ ___   __ _  ___ ___    ___  __ _ _ __| |_   _
;;  / _ \ '_ ` _ \ / _` |/ __/ __|  / _ \/ _` | '__| | | | |
;; |  __/ | | | | | (_| | (__\__ \ |  __/ (_| | |  | | |_| |
;;  \___|_| |_| |_|\__,_|\___|___/  \___|\__,_|_|  |_|\__, |
;;                                                    |___/
;; ============================================================

;; disable package.el at startup
(setq package-enable-at-startup nil)

;; set alias to y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; configure font size
(set-face-attribute 'default nil :family "Iosevka" :height 120)

;; Do not resize at early stage
(setq frame-inhibit-implied-resize t)

;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 4)
(scroll-bar-mode -1)

;; disable statup messages
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq inhibit-startup-buffer-menu t)

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
