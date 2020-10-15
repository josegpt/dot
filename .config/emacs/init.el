(require 'package)

;; MELPA
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; INIT PACKAGES
(package-initialize)

;; GETTING FRESH PACKAGES
(when (not package-archive-contents)
  (package-refresh-contents))

;; MY PACKAGES
(defvar my-packages
'(
  company
  counsel
  doom-modeline
  material-theme
  expand-region
  exwm
  linum-relative
  magit
  move-text
  projectile
  rainbow-delimiters
) "A list of packages to ensure are installed at launch.")

;; AUTO INSTALL PACKAGES
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; CONFIG PATH
(add-to-list 'load-path "~/.config/emacs/settings")

;; EXWM
(load "wm.el")

;; BASE CONFIGS
(load "base.el")

;; SETUP PACKAGES
(load "setup-packages.el")

;; SETUP LANGUAGES

;; ELISP
(load "setup-elisp.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(geiser parinfer-rust-mode projectile paredit cider web-mode tide smartparens rainbow-delimiters powerline move-text magit linum-relative exwm expand-region darkokai-theme counsel company badwolf-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
