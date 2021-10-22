;; Disable package.el at startup
(setq package-enable-at-startup nil)

;; Set alias to y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)
;; (setq use-short-answers t) emacs28

;; Configure font size
(set-face-attribute 'default nil :family "Dina" :height 120)

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
;; (setq native-comp-async-report-warnings-errors 'silent) emacs28
