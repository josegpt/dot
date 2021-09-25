;; Set alias to y-or-n
(setq use-short-answers t) 

;; Configure font size
(set-face-attribute 'default nil :family "Iosevka" :height 120)

;; Do not resize at early stage
(setq frame-inhibit-implied-resize t)

;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 4)
(scroll-bar-mode -1)

;; Disable statup messages
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq inhibit-startup-buffer-menu t)
(setq native-comp-async-report-warnings-errors 'silent)
