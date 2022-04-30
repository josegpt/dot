;; Initialise installed packages
(setq package-enable-at-startup t)

;; Set alias to y-or-n
(setq use-short-answers t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Do not resize at early stage
(setq frame-inhibit-implied-resize t)

;; Disable GUI elements
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(set-fringe-mode 10)

;; Disable statup messages
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq inhibit-startup-buffer-menu t)
