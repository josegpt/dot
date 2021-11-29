;;; init.el --- description -*- lexical-binding: t -*-
;;;;;;;;;;
;; Init ;;
;;;;;;;;;;

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; make use-package use straight
(straight-use-package 'use-package)

;;; set straight as default
(setq straight-use-package-by-default 1)

;; debugging purposes
;; (setq use-package-verbose t)

;;; the default is 800 kilobytes. measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(add-to-list 'load-path (concat user-emacs-directory
                                (convert-standard-filename "lisp/")))

;; native-compile all Elisp files under a directory
(native-compile-async (concat user-emacs-directory
                              (convert-standard-filename "lisp/")) 'recursively)

;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(use-package auth-source-pass
  :straight (:type built-in)
  :after password-store
  :config
  (auth-source-pass-enable))

(use-package autorevert
  :straight (:type built-in)
  :custom
  (global-auto-revert-non-file-buffers nil)
  :config
  (global-auto-revert-mode))

(use-package compile
  :custom
  (compilation-scroll-output t))

(use-package corfu
  :hook
  ((prog-mode shell-mode eshell-mode ledger-mode) . corfu-mode)
  :custom
  (corfu-cycle t))

(use-package css
  :straight (:type built-in)
  :mode
  ("\\.\\(css\\|less\\|sass\\|scss\\|styl\\)\\'" . css-mode))

(use-package diff-hl
  :config
  (global-diff-hl-mode))

(use-package dired
  :straight (:type built-in)
  :custom
  (dired-kill-when-opening-new-dired-buffer t))

(use-package dockerfile-mode
   :mode "\\Dockerfile\\'")

(use-package display-line-numbers
  :straight (:type built-in)
  :hook ((prog-mode html-mode ledger-mode) . display-line-numbers-mode)
  :custom
  (display-line-numbers-type 'relative)
  (display-line-numbers-current-absolute t))

(use-package envrc
  :config
  (envrc-global-mode))

(use-package elm-mode
  :mode "\\.elm\\'"
  :hook
  (elm-mode . elm-indent-mode)
  (elm-mode . elm-format-on-save-mode))

(use-package elfeed
  :bind
  ("C-c r" . elfeed)
  :custom
  (elfeed-use-curl t)
  (elfeed-db-directory "~/.cache/elfeed")
  (elfeed-search-title-max-width 100)
  (elfeed-search-title-min-width 100)
  (elfeed-feeds '(("https://reddit.com/r/Gentoo.rss" linux)
                  ("https://reddit.com/r/emacs.rss" emacs)
                  ("https://reddit.com/r/unixporn.rss" linux)
                  ("http://feeds.feedburner.com/crunchyroll/rss/anime" anime)
                  ("https://sachachua.com/blog/category/emacs-news/feed" emacs news))))

(use-package erc
  :straight (:type built-in)
  :bind
  ("C-c i" . erc-tls)
  :custom
  (erc-server "irc.us.libera.chat")
  (erc-nick "josegpt")
  (erc-user-full-name "Jose G Perez Taveras")
  (erc-track-shorten-start 8)
  (erc-kill-buffer-on-part t)
  (erc-auto-query 'bury)
  (erc-autojoin-channels-alist '(("irc.libera.chat" "#emacs" "#systemcrafters"))))

(use-package emacs
  :straight (:type built-in)
  :config
  (load-theme 'modus-vivendi t)
  (set-face-attribute 'mode-line-active nil :inherit 'mode-line)
  (add-to-list 'default-frame-alist '(alpha . (85 . 85)))
  (set-frame-parameter (selected-frame) 'alpha '(85 . 85))
  :custom
  (tab-width 2)
  (indent-tabs-mode nil)
  (ring-bell-function 'ignore)
  ;; don't clutter up directories with files~
  (backup-directory-alist `((".*" . ,temporary-file-directory)))
  ;; don't clutter with #files either
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t))))

(use-package eww
  :straight (:type built-in)
  :custom
  (eww-auto-rename-buffer t))

(use-package frame
  :straight (:type built-in)
  :custom
  (blink-cursor-mode nil))

(use-package go-mode
  :mode "\\.go\\'")

(use-package haskell-mode
  :mode "\\.hs\\'")

(use-package html
  :straight (:type built-in)
  :mode
  ("\\.\\(html?\\|ejs\\)\\'" . html-mode))

(use-package hl-line
  :straight (:type built-in)
  :config
  (global-hl-line-mode))

(use-package vertico
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode))

;; emacs28
;; (use-package icomplete
;;   :config
;;   (icomplete-mode)
;;   (fido-vertical-mode)
;;   :custom
;;   (icomplete-compute-delay 0.0)
;;   (icomplete-delay-completions-threshold 200))

(use-package js
  :straight (:type built-in)
  :mode
  ("\\.js\\'" . js-mode)
  :custom
  (js-indent-level 2))

(use-package ledger-mode
  :mode "\\.\\(ledger\\|dat\\)\\'"
  :bind
  (:map ledger-mode-map
        ("C-M-i" . completion-at-point))
  :custom
  (ledger-complete-in-steps t)
  (ledger-clear-whole-transactions t)
  (ledger-reports '(("bal" "%(binary) -f %(ledger-file) bal")
                    ("reg" "%(binary) -f %(ledger-file) reg")
                    ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
                    ("account" "%(binary) -f %(ledger-file) reg %(account)")
                    ("net worth" "%(binary) -f %(ledger-file) bal ^assets ^liabilities")
                    ("cash flow" "%(binary) -f %(ledger-file) bal ^income ^equity ^expenses"))))

(use-package magit
  :bind
  ("C-x g" . magit-status)
  :custom
  (magit-clone-default-directory "~/projects/")
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package marginalia
  :init (marginalia-mode)
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle)))

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package minibuffer
  :straight (:type built-in)
  :custom
  (completion-ignore-case t)
  (completion-cycle-threshold 3)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(partial-completion substring)))

(use-package move-text
  :bind
  ("M-p" . move-text-up)
  ("M-n" . move-text-down))

(use-package nov
  :mode
  ("\\.epub\\'" . nov-mode)
  :custom
  (nov-text-width 80))

(use-package otaku
  :straight (:type built-in)
  :bind
  ("C-c a" . otaku-search-anime))

(use-package orderless
  :after minibuffer
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package paren
  :straight (:type built-in)
  :hook (prog-mode . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t))

(use-package password-store
  :bind
  ("C-c p e" . password-store-edit)
  ("C-c p w" . password-store-copy)
  ("C-c p c" . password-store-clear)
  ("C-c p i" . password-store-insert)
  ("C-c p r" . password-store-rename)
  ("C-c p k" . password-store-remove)
  ("C-c p g" . password-store-generate)
  ("C-c p f" . password-store-copy-field))

(use-package pinentry
  :config
  (pinentry-start)
  :custom
  (epg-pinentry-mode 'loopback))

(use-package project
  :bind
  ("C-x p m" . magit-project-status)
  ("C-x p a" . envrc-allow))

(use-package proced
  :custom
  (proced-auto-update-timer 1)
  :bind
  ("C-c d" . proced))

(use-package prettier-js
  :hook ((html-mode
          js-mode
          typescript-mode
          css-mode
          markdown-mode
          vue-mode
          yaml-mode) . prettier-js-mode))

(use-package simple
  :straight (:type built-in)
  :bind
  ("C-c l" . list-processes))

(use-package subword
  :hook ((js-mode
          go-mode
          elm-mode
          haskell-mode
          typescript-mode
          ledger-mode) . subword-mode))

(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :custom
  (typescript-indent-level 2))

(use-package tooltip
  :straight (:type built-in)
  :custom
  (tooltip-mode nil))

(use-package webjump
  :bind
  ("C-c j" . webjump)
  :custom
  (webjump-sites '(("Gmail" . "mail.google.com")
                           ("Discord" . "discord.com/app")
                           ("Telegram" . "web.telegram.org")
                           ("WhatsApp" . "web.whatsapp.com")
                           ("Melpa" . [simple-query "melpa.org" "melpa.org/#/?q=" ""])
                           ("Amazon" . [simple-query "amazon.com" "amazon.com/s?k=" ""])
                           ("Reddit Sub" . [simple-query "reddit.com" "reddit.com/r/" ""])
                           ("Google" . [simple-query "google.com" "google.com/search?q=" ""])
                           ("Github" . [simple-query "github.com" "github.com/search?q=" ""])
                           ("Ebay" . [simple-query "ebay.com" "ebay.com/sch/i.html?_nkw=" ""])
                           ("Twitch" . [simple-query "twitch.tv" "twitch.tv/search?term=" ""])
                           ("Reddit" . [simple-query "reddit.com" "reddit.com/search/?q=" ""])
                           ("Wikipedia" . [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
                           ("Gitlab" . [simple-query "gitlab.com" "gitlab.com/search?search=" ""])
                           ("DuckDuckGo" . [simple-query "duckduckgo.com" "duckduckgo.com/?q=" ""])
                           ("AnimeFLV" . [simple-query "animeflv.net" "animeflv.net/browse?q=" ""])
                           ("Gitlab User & Repository" . [simple-query "gitlab.com" "gitlab.com/" ""])
                           ("Github User & Repository" . [simple-query "github.com" "github.com/" ""])
                           ("Youtube" . [simple-query "youtube.com" "youtube.com/results?search_query=" ""])
                           ("Crunchyroll" . [simple-query "crunchyroll.com" "crunchyroll.com/search?&q=" ""])
                           ("Elpa" . [simple-query "elpa.gnu.org/packages/" "elpa.gnu.org/packages/" ".html"])
                           ("Youtube Music" . [simple-query "music.youtube.com" "music.youtube.com/search?q=" ""])
                           ("Gentoo Packages" . [simple-query "packages.gentoo.org" "packages.gentoo.org/packages/search?q=" ""]))))

(use-package whitespace
  :straight (:type built-in)
  :hook (prog-mode . whitespace-mode)
  :custom
  (whitespace-style '(face
                      tabs
                      empty
                      spaces
                      newline
                      tab-mark
                      trailing
                      space-mark
                      indentation
                      space-after-tab
                      space-before-tab)))

(use-package which-key
  :config
  (which-key-mode)
  :custom
  (which-key-idle-delay 0.3))

(use-package woman
  :bind
  ("C-c w" . woman))

(use-package window
  :straight (:type built-in)
  :custom
  (display-buffer-alist '(("\\`\\*Async Shell Command\\*\\'"
                           (display-buffer-no-window))
                          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|compilation\\)\\*"
                           (display-buffer-in-side-window)
                           (window-height . 0.18)
                           (side . bottom)
                           (slot . 0))
                          ("\\*\\(Ledger.*\\|Woman.*\\|Man.*\\|Help.*\\)\\*"
                           (display-buffer-reuse-mode-window display-buffer-in-side-window)
                           (window-width . 0.36)
                           (side . right)
                           (slot . -1))
                          ("\\*\\(envrc\\|Process List\\|Proced\\|Buffer List\\)\\*"
                           (display-buffer-reuse-mode-window display-buffer-in-side-window)
                           (window-height . 0.18)
                           (side . bottom)
                           (slot . -1)))))

(use-package vue-mode
  :mode "\\.vue\\'")

(use-package yaml-mode
  :mode "\\.ya?lm\\'")

(use-package yasnippet
  :config
  (yas-global-mode))
;;; init.el ends here
