;; Disable package.el at startup
(setq package-enable-at-startup nil)

;; Set alias to y-or-n
(setq use-short-answers t)

;; Configure font size
(set-face-attribute 'default nil :family "Victor Mono" :height 140)

;; Do not resize at early stage
(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)

;; Deferred native comp
(setq native-comp-deferred-compilation t)

;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 10)
(scroll-bar-mode -1)

;; Disable statup messages
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq inhibit-startup-buffer-menu t)
(setq native-comp-async-report-warnings-errors 'silent)
