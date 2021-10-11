;;; init.el --- My init file. -*- lexical-binding: t -*-

;;; debugging
;; (setq use-package-verbose t)

;;; the default is 800 kilobytes. measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
(add-hook 'emacs-startup-hook (lambda ()
                                (message "---> Emacs loaded in %s with %d garbage collections."
                                         (format "%.2f seconds"
                                                 (float-time
                                                  (time-subtract after-init-time before-init-time)))
                                         gcs-done)))

(add-to-list 'load-path (concat user-emacs-directory
                                (convert-standard-filename "elisp/")))

;; ============================================================
;; Packages
;; ============================================================

(use-package auth-source-pass
  :after password-store
  :config
  (auth-source-pass-enable))

(use-package autorevert
  :custom
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode))

(use-package battery
  :unless (string= (system-name) "guts")
  :init (display-battery-mode))

(use-package bookmark
  :bind
  ("s-l" . bookmark-jump))

(use-package compile
  :custom
  (compilation-scroll-output t)
  (compilation-shell-minor-mode t))

(use-package corfu
  :hook
  ((prog-mode
    shell-mode
    eshell-mode
    ledger-mode) . corfu-mode)
  :custom
  (corfu-cycle t))

(use-package daemons
  :bind
  ("C-c d" . daemons))

(use-package envrc
  :config
  (envrc-global-mode))

(use-package dired
  :bind
  ("s-d" . dired)
  :custom
  (dired-kill-when-opening-new-dired-buffer t))

(use-package display-line-numbers
  :hook
  ((prog-mode
    html-mode
    ledger-mode) . display-line-numbers-mode)
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
  :config
  (require 'pt-elfeed)
  :bind
  ("s-r" . elfeed)
  (:map elfeed-search-mode-map
        ("w" . pt-elfeed-play-youtube-link))
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
  :bind
  ("s-i" . erc-tls)
  :custom
  (erc-server "irc.us.libera.chat")
  (erc-nick "josegpt")
  (erc-user-full-name "Jose G Perez Taveras")
  (erc-track-shorten-start 8)
  (erc-kill-buffer-on-part t)
  (erc-auto-query 'bury)
  (erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs"))))

(use-package emacs
  :init (server-mode)
  :config
  (load-theme 'modus-operandi t)
  :custom
  (indent-tabs-mode nil)
  (tab-width 2)
  (ring-bell-function 'ignore)
  ;; don't clutter up directories with files~
  (backup-directory-alist `((".*" . ,temporary-file-directory)))
  ;; don't clutter with #files either
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t))))

(use-package eshell
  :bind
  ("<s-return>" . eshell))

(use-package frame
  :custom
  (blink-cursor-mode nil))

(use-package files
  :bind
  ("s-f" . find-file)
  ("s-s" . save-buffer))

(use-package hl-line
  :config
  (global-hl-line-mode))

(use-package icomplete
  :config
  (icomplete-mode)
  (fido-vertical-mode)
  :custom
  (icomplete-compute-delay 0.0)
  (icomplete-delay-completions-threshold 200))

(use-package magit
  :bind
  ("s-g" . magit-status)
  :custom
  (magit-clone-default-directory "~/projects/")
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package minibuffer
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

(use-package mu4e
  :load-path "/home/josegpt/.guix-extra-profiles/base/base/share/emacs/site-lisp/"
  :defer 30
  :bind
  ("s-m" . mu4e)
  :custom
  (mu4e-change-filenames-when-moving t)
  (mu4e-view-show-addresses t)
  (mu4e-update-interval (* 30 60))
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-view-show-images t)
  (mu4e-sent-messages-behavior 'delete)
  (mu4e-confirm-quit nil)
  (message-kill-buffer-on-exit t)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-attachment-dir "~/Downloads")
  (user-full-name "Jose G Perez Taveras")
  (user-mail-address "josegpt27@gmail.com")
  (mu4e-maildir "~/Mail")
  (mu4e-sent-folder "/Sent Mail")
  (mu4e-drafts-folder "/Drafts")
  (mu4e-trash-folder "/Trash")
  (mu4e-maildir-shortcuts
   '((:maildir "/INBOX" :key ?i)
     (:maildir "/Sent Mail" :key ?s)
     (:maildir "/Starred" :key ?r)
     (:maildir "/Spam" :key ?p)
     (:maildir "/Drafts" :key ?d)
     (:maildir "/Trash" :key ?t)))
  ;; Send Emails
  ;; FIXME: Add authinfo.gpg
  (mail-user-agent 'mu4e-user-agent)
  (message-send-mail-function 'smtpmail-send-it)
  (smtpmail-smtp-user "josegpt27")
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 465)
  (smtpmail-stream-type 'ssl)
  :config
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  (mu4e t))

(use-package project
  :config
  (require 'pt-project)
  :bind
  ("s-p" . pt-project-switch-commands-current-project)
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

(use-package paren
  :custom
  (show-paren-when-point-inside-paren t)
  :hook (prog-mode . show-paren-mode))

(use-package proced
  :custom
  (proced-auto-update-timer 1)
  :bind
  ("C-c r" . proced))

(use-package prettier-js
  :hook ((html-mode
          js-mode
          typescript-mode
          css-mode
          markdown-mode
          yaml-mode) . prettier-js-mode))

(use-package pinentry
  :after minibuffer
  :config
  (pinentry-start)
  :custom
  (epg-pinentry-mode 'loopback))

(use-package pt-desktop
  :bind
  ("s-a" . pt-desktop-powersettings)
  ("<XF86AudioRaiseVolume>" . pt-desktop-audio-volume-increment)
  ("<XF86AudioLowerVolume>" . pt-desktop-audio-volume-decrement)
  ("<XF86AudioMute>" . pt-desktop-audio-mute-toggle)
  ("<s-XF86AudioRaiseVolume>" . pt-desktop-audio-mic-volume-increment)
  ("<s-XF86AudioLowerVolume>" . pt-desktop-audio-mic-volume-decrement)
  ("<XF86AudioMicMute>" . pt-desktop-audio-mic-mute-toggle)
  ("<XF86MonBrightnessUp>" . pt-desktop-brightness-increment)
  ("<XF86MonBrightnessDown>" . pt-desktop-brightness-decrement))

(use-package subword
  :hook ((js-mode
          go-mode
          elm-mode
          typescript-mode) . subword-mode))

(use-package time
  :custom
  (display-time-format "(%A) %b %d, %Y | %I:%M%P")
  :config
  (display-time-mode))

(use-package tooltip
  :custom
  (tooltip-mode nil))

(use-package webjump
  :bind
  ("s-j" . webjump)
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
  :bind
  ("s-SPC" . whitespace-mode)
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
  :bind
  ("s-0" . delete-window)
  ("s-1" . delete-other-windows)
  ("s-2" . split-window-below)
  ("s-3" . split-window-right)
  ("s-o" . other-window)
  ("s-c" . kill-current-buffer)
  ("s-C" . kill-buffer-and-window)
  ("s-b" . switch-to-buffer)
  ("s-k" . kill-buffer)
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
  :mode
  ("\\.\\(html?\\|ejs\\)\\'" . html-mode))

(use-package go-mode
  :mode "\\.go\\'")

(use-package js
  :mode
  ("\\.js\\'" . js-mode)
  :custom
  (js-indent-level 2))

(use-package ledger-mode
  :mode "\\.\\(ledger\\|dat\\)\\'"
  :bind
  ("TAB" . completion-at-point)
  :custom
  (ledger-complete-in-steps t)
  (ledger-clear-whole-transactions t))

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package nov
  :mode
  ("\\.epub\\'" . nov-mode)
  :custom
  (nov-text-width 80))

(use-package pdf-view
  :mode
  ("\\.pdf\\'" . pdf-view-mode))

(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :custom
  (typescript-indent-level 2))

(use-package yaml-mode
  :mode "\\.ya?lm\\'")

(use-package exwm-randr
  :when (string= (system-name) "guts")
  :after exwm
  :hook
  (exwm-randr-screen-change . pt-exwm-xrandr-config)
  :config
  (exwm-randr-enable)
  :custom
  (exwm-randr-workspace-monitor-plist '(0 "HDMI-1-1" 1 "HDMI-1-2" 2 "HDMI-1-3" 3 "HDMI-1-4" 4 "HDMI-1-5")))

(use-package exwm
  :init (exwm-enable)
  :config
  (require 'pt-exwm)
  :hook
  (exwm-update-class . pt-exwm-rename-buffer-with-class-name)
  (exwm-update-title . pt-exwm-custom-rename-buffer-with-title)
  (exwm-manage-finish . pt-exwm-send-window-to-workspace)
  :bind
  (:map exwm-mode-map
        ("C-q" . exwm-input-send-next-key))
  :custom
  (exwm-workspace-number 5)
  (exwm-workspace-switch-create-limit 5)
  (exwm-workspace-warp-cursor t)
  (exwm-input-prefix-keys '(?\C-x
                            ?\C-c
                            ?\C-u
                            ?\C-h
                            ?\C-g
                            ?\M-x
                            ?\M-:
                            ?\M-!
                            ?\s-l
                            ?\s-d
                            ?\s-r
                            ?\s-f
                            ?\s-s
                            ?\s-g
                            ?\s-m
                            ?\s-a
                            ?\s-j
                            ?\s-0
                            ?\s-1
                            ?\s-2
                            ?\s-3
                            ?\s-o
                            ?\s-c
                            ?\s-C
                            ?\s-b
                            ?\s-k
                            ?\s-a
                            s-return
                            XF86AudioRaiseVolume
                            XF86AudioLowerVolume
                            s-XF86AudioRaiseVolume
                            s-XF86AudioLowerVolume
                            XF86AudioMute
                            XF86AudioMicMute
                            XF86MonBrightnessUp
                            XF86MonBrightnessDown))
  (exwm-input-global-keys
   `(([?\s-w ?q] . exwm-reset)
     ([?\s-w ?s] . exwm-workspace-switch)
     ([?\s-w ?r] . pt-exwm-run-app)
     ,@(mapcar (lambda (i)
                 `(,(kbd (format "C-s-%d" (1+ i))) .
                   (lambda ()
                     (interactive)
                     (exwm-workspace-switch-create ,i))))
               (number-sequence 0 4))))
  (exwm-input-simulation-keys
   '(([?\C-b] . [left])
     ([?\C-f] . [right])
     ([?\C-p] . [up])
     ([?\C-n] . [down])
     ([?\C-a] . [home])
     ([?\C-e] . [end])
     ([?\C-v] . [next])
     ([?\M-v] . [prior])
     ([?\M-b] . [C-left])
     ([?\M-f] . [C-right])
     ([?\M-<] . [home])
     ([?\M->] . [end])
     ([?\C-d] . [delete])
     ([?\C-w] . [?\C-x])
     ([?\M-w] . [?\C-c])
     ([?\C-y] . [?\C-v])
     ([?\C-s] . [?\C-f])
     ([?\C-c ?h] . [?\C-a])
     ([?\C-c ?f] . [?\C-l])
     ([?\C-c ?k] . [?\C-w])
     ([?\C-c ?g] . [escape])
     ([?\C-\M-b] . [M-left])
     ([?\C-\M-f] . [M-right])
     ([?\C-k] . [S-end C-x])
     ([?\M-d] . [C-S-right C-x])
     ([?\C-0 ?\C-k] . [C-S-home C-x])
     ([M-backspace] . [C-S-left C-x]))))
;;; init.el ends here
