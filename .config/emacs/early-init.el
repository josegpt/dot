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
