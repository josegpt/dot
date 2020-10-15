;; THEME
(load-theme 'material t)

;; HIGHLIGHTS MATCHING PAREN
(show-paren-mode 1)

;; NO LOCKFILES
(setq create-lockfiles nil)

;; NO BLINKING
(blink-cursor-mode 0)

;; GUI
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(set-frame-font "FiraCode 10" nil t)

;; BACKUP
(setq backup-directory-alist '(("." . "~/.cache/emacs/undodir")))
(setq backup-by-copying t)
