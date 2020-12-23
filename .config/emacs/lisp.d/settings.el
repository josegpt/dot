;; matching paren
(show-paren-mode 1)

;; no blinking
(blink-cursor-mode 0)

;; disable menu bar
(menu-bar-mode -1)

;; disable tool bar
(tool-bar-mode -1)

;; more space
(set-fringe-mode 8)

;; disbale tooltip
(tooltip-mode -1)

;; disable scroll bars
(scroll-bar-mode -1)
(toggle-scroll-bar -1)

;; auto refresh changed file
(global-auto-revert-mode t)

;; disable statup messages
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

;; activate line number
(column-number-mode)
(global-display-line-numbers-mode t)

;; initial load elisp-mode
(setq initial-major-mode 'emacs-lisp-mode)

;; Don't clutter up directories with files~
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

;; Don't clutter with #files either
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; enable emacs daemon
(server-mode)
