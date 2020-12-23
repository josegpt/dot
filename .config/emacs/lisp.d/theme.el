;; configure font size
(set-face-attribute 'default nil :height 110)

;; remove modes from modeline
(use-package diminish)

;; rainbow-delimiters
(use-package rainbow-delimiters
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

;; theme
(use-package doom-themes
  :config
  (load-theme 'doom-Iosvkem t))
