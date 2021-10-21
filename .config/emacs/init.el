;; ============================================================
;; Init
;; ============================================================

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

(add-hook 'emacs-startup-hook (lambda ()
                                (message "---> Emacs loaded in %s with %d garbage collections."
                                         (format "%.2f seconds"
                                                 (float-time
                                                  (time-subtract after-init-time before-init-time)))
                                         gcs-done)))

;; ============================================================
;; Packages
;; ============================================================

(use-package auth-source-pass
  :straight (:type built-in)
  :after password-store
  :config
  (auth-source-pass-enable))

(use-package autorevert
  :straight (:type built-in)
  ;; emacs28
  ;; :custom
  ;; (global-auto-revert-non-file-buffers nil)
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

;; (use-package envrc
;;   :config
;;   (envrc-global-mode))

;; emacs28
;; (use-package dired
;;   :custom
;;   (dired-kill-when-opening-new-dired-buffer t))

(use-package display-line-numbers
  :straight (:type built-in)
  :hook ((prog-mode html-mode ledger-mode) . display-line-numbers-mode)
  :custom
  (display-line-numbers-type 'relative)
  (display-line-numbers-current-absolute t))

(use-package eglot
  :bind
  (:map eglot-mode-map
        ("C-c e h" . eldoc)
        ("C-c e r" . eglot-rename)
        ("C-c e f" . eldoc-format-buffer)
        ("C-c e o" . eglot-code-action-organize-imports))
  :config
  (add-to-list 'eglot-server-programs
               '((js-mode typescript-mode)
                 "typescript-language-server" "--stdio")))

(use-package elfeed
  :bind
  ("C-c r" . elfeed)
  :custom
  (elfeed-use-curl t)
  (elfeed-db-directory "~/.cache/elfeed")
  (elfeed-search-title-max-width 100)
  (elfeed-search-title-min-width 100)
  (elfeed-feeds '(("https://reddit.com/r/emacs.rss" emacs)
                  ("https://reddit.com/r/unixporn.rss" linux)
                  ("https://reddit.com/r/guix.rss" linux)
                  ("http://feeds.feedburner.com/crunchyroll/rss/anime" anime)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCBJycsmduvYEL83R_U4JriQ" youtube tech)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC-lHJZR3Gqxm24_Vd_AJ5Yw" youtube funny)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCld68syR8Wi-GY_n4CaoJGA" youtube vim)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCLqH-U2TXzj1h7lyYQZLNQQ" youtube fitness)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVls1GmFKf6WlTraIb_IaJg" youtube emacs unix vim)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA" youtube vim unix)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC8ENHE5xdFSwx71u3fDH5Xw" youtube vim programming)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCrqM0Ym_NbK1fqeQG2VIohg" youtube emacs programming)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ" youtube emacs exwm programming))))

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
  (erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs"))))

(use-package emacs
  :straight (:type built-in)
  :custom
  (indent-tabs-mode nil)
  (tab-width 2)
  (ring-bell-function 'ignore)
  ;; don't clutter up directories with files~
  (backup-directory-alist `((".*" . ,temporary-file-directory)))
  ;; don't clutter with #files either
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t))))

(use-package frame
  :straight (:type built-in)
  :custom
  (blink-cursor-mode nil))

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium t))

(use-package hl-line
  :straight (:type built-in)
  :config
  (global-hl-line-mode))

;; emacs28
;; (use-package icomplete
;;   :config
;;   (icomplete-mode)
;;   (fido-vertical-mode)
;;   :custom
;;   (icomplete-compute-delay 0.0)
;;   (icomplete-delay-completions-threshold 200))

(use-package magit
  :bind
  ("C-x g" . magit-status)
  :custom
  (magit-clone-default-directory "~/projects/")
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package marginalia
  :after minibuffer
  :custom
  (marginalia-margin-threshold 150)
  :config
  (marginalia-mode))

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

(use-package orderless
  :after vertico
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
  :after minibuffer
  :config
  (pinentry-start)
  :custom
  (epg-pinentry-mode 'loopback))

(use-package project
  :custom
  (project-switch-commands '((?f "File" project-find-file)
                             (?g "Grep" project-find-regexp)
                             (?d "Dired" project-dired)
                             (?b "Buffer" project-switch-to-buffer)
                             (?c "Compile" project-compile)
                             (?q "Query replace" project-query-replace-regexp)
                             (?m "Magit" magit-project-status)
                             (?v "VC dir" project-vc-dir)
                             (?e "Eshell" project-eshell)
                             (?! "Shell command" project-shell-command)
                             (?l "Eglot LSP" eglot)
                             (?a "Envrc Allow" envrc-allow)
                             (?r "Envrc Reload" envrc-reload))))

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
          yaml-mode) . prettier-js-mode))

(use-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode))

(use-package select
  :custom
  (x-select-enable-clipboard t))

(use-package subword
  :hook ((js-mode
          go-mode
          elm-mode
          haskell-mode
          typescript-mode) . subword-mode))

(use-package tooltip
  :straight (:type built-in)
  :custom
  (tooltip-mode nil))

(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-cycle t))

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
                   ("Crunchyroll" . [simple-query "crunchyroll.com" "crunchyroll.com/search?&q=" ""]))))

(use-package whitespace
  :straight (:type built-in)
  :bind
  ("C-c SPC" . whitespace-mode)
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

(use-package window
  :straight (:type built-in)
  :no-require t
  :custom
  (display-buffer-alist '(("\\`\\*Async Shell Command\\*\\'"
                           (display-buffer-no-window))
                          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|compilation\\|envrc\\)\\*"
                           (display-buffer-in-side-window)
                           (window-height . 0.25)
                           (side . bottom)
                           (slot . 1))
                          ("\\*\\(Help.*\\|Ledger.*\\)\\*"
                           (display-buffer-in-side-window)
                           (window-width . 0.35)
                           (side . right)
                           (slot . -1))
                          ("\\*.*e?shell.*"
                           (display-buffer-reuse-mode-window display-buffer-at-bottom)
                           (window-height . 0.25)))))

(use-package yasnippet
  :config
  (yas-global-mode))

(use-package css
  :straight (:type built-in)
  :mode
  ("\\.\\(css\\|less\\|sass\\|scss\\|styl\\)\\'" . css-mode))

(use-package dockerfile-mode
   :mode "\\Dockerfile\\'")

(use-package elm-mode
  :mode "\\.elm\\'"
  :hook (elm-mode . elm-format-on-save-mode))

(use-package haskell-mode
  :mode "\\.hs\\'")

(use-package html
  :straight (:type built-in)
  :mode
  ("\\.\\(html?\\|ejs\\)\\'" . html-mode))

(use-package go-mode
  :mode "\\.go\\'")

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

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package nov
  :mode
  ("\\.epub\\'" . nov-mode)
  :custom
  (nov-text-width 80))

(use-package pdf-tools
  :mode
  ("\\.pdf\\'" . pdf-view-mode))

(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :custom
  (typescript-indent-level 2))

(use-package yaml-mode
  :mode "\\.ya?lm\\'")

;; (use-package exwm-randr
;;   :straight nil
;;   :if (string= system-name "josegpt-desktop")
;;   :after exwm
;;   :hook
;;   (exwm-randr-screen-change . pt-exwm-xrandr-config)
;;   :config
;;   (exwm-randr-enable)
;;   :custom
;;   (exwm-randr-workspace-monitor-plist '(0 "HDMI-1-1" 1 "HDMI-1-2" 2 "HDMI-1-3" 3 "HDMI-1-4" 4 "HDMI-1-5")))

;; (use-package exwm
;;   :init (exwm-enable)
;;   :if (string= system-type "gnu/linux")
;;   :config
;;   (require 'pt-exwm)
;;   :hook
;;   (exwm-update-class . pt-exwm-rename-buffer-with-class-name)
;;   (exwm-update-title . pt-exwm-custom-rename-buffer-with-title)
;;   (exwm-manage-finish . pt-exwm-send-window-to-workspace)
;;   :bind
;;   (:map exwm-mode-map
;;         ("C-q" . exwm-input-send-next-key))
;;   :custom
;;   (exwm-workspace-number 5)
;;   (exwm-workspace-warp-cursor t)
;;   (exwm-input-prefix-keys
;;    '(?\C-x
;;      ?\C-c
;;      ?\C-u
;;      ?\C-h
;;      ?\C-g
;;      ?\M-x
;;      ?\M-:
;;      ?\M-!
;;      ?\s-q
;;      ?\s-j
;;      XF86AudioRaiseVolume
;;      XF86AudioLowerVolume
;;      s-XF86AudioRaiseVolume
;;      s-XF86AudioLowerVolume
;;      XF86AudioMute
;;      XF86AudioMicMute
;;      XF86MonBrightnessUp
;;      XF86MonBrightnessDown))
;;   (exwm-input-global-keys
;;    `(([?\s-r] . exwm-reset)
;;      ([?\s-w] . exwm-workspace-switch)
;;      ([?\s-&] . pt-exwm-run-app)
;;      ,@(mapcar (lambda (i)
;;                  `(,(kbd (format "s-%d" (1+ i))) .
;;                    (lambda ()
;;                      (interactive)
;;                      (exwm-workspace-switch-create ,i))))
;;                (number-sequence 0 4))))
;;   (exwm-input-simulation-keys
;;    '(([?\C-b] . [left])
;;      ([?\C-f] . [right])
;;      ([?\C-p] . [up])
;;      ([?\C-n] . [down])
;;      ([?\C-a] . [home])
;;      ([?\C-e] . [end])
;;      ([?\C-v] . [next])
;;      ([?\M-v] . [prior])
;;      ([?\M-b] . [C-left])
;;      ([?\M-f] . [C-right])
;;      ([?\M-<] . [home])
;;      ([?\M->] . [end])
;;      ([?\C-d] . [delete])
;;      ([?\C-w] . [?\C-x])
;;      ([?\M-w] . [?\C-c])
;;      ([?\C-y] . [?\C-v])
;;      ([?\C-s] . [?\C-f])
;;      ([?\C-c ?h] . [?\C-a])
;;      ([?\C-c ?f] . [?\C-l])
;;      ([?\C-c ?k] . [?\C-w])
;;      ([?\C-c ?g] . [escape])
;;      ([?\C-\M-b] . [M-left])
;;      ([?\C-\M-f] . [M-right])
;;      ([?\C-k] . [S-end delete])
;;      ([M-backspace] . [C-backspace])
;;      ([?\M-d] . [C-S-right delete]))))
