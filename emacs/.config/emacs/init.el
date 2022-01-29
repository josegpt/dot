;;; init.el --- description -*- lexical-binding: t -*-
;;; Package.el

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; debugging
;; (setq use-package-verbose t)

;;; the default is 800 kilobytes. measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;;; include custom elisp
(add-to-list 'load-path (concat user-emacs-directory
                                (convert-standard-filename "lisp/")))

;; native-compile all Elisp files under a directory
;; (native-compile-async (concat user-emacs-directory
;;                               (convert-standard-filename "lisp/")) 'recursively)

;;; Packages

(use-package auth-source-pass
  :config
  (auth-source-pass-enable))

(use-package autorevert
  :custom
  (global-auto-revert-non-file-buffers nil)
  :config
  (global-auto-revert-mode))

(use-package battery
  :unless (string= (system-name) "josegpt-desktop")
  :config
  (display-battery-mode))

(use-package bookmark
  :bind
  ("C-c b" . bookmark-jump))

(use-package browse-url
  :custom
  (browse-url-browser-function 'eww-browse-url)
  (browse-url-secondary-browser-function 'browse-url-default-browser)
  (browse-url-handlers '((".*\\(youtube\\|twitch\\|discord\\|whatsapp\\|telegram\\|google\\)" . browse-url-default-browser))))

(use-package canales
  :bind
  ("C-c c" . canales-watch))

(use-package compile
  :custom
  (compilation-scroll-output t))

(use-package corfu
  :ensure t
  :hook ((prog-mode shell-mode eshell-mode ledger-mode) . corfu-mode)
  :custom
  (corfu-cycle t))

(use-package css-mode
  :mode "\\.\\(css\\|less\\|sass\\|scss\\|styl\\)\\'")

(use-package dired
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-alh")
  (dired-kill-when-opening-new-dired-buffer t))

(use-package dockerfile-mode
  :ensure t
  :mode "\\Dockerfile\\'")

(use-package display-line-numbers
  :hook ((prog-mode html-mode conf-mode ledger-mode) . display-line-numbers-mode)
  :custom
  (display-line-numbers-type 'relative)
  (display-line-numbers-current-absolute t))

(use-package eshell
  :bind
  ("s-<return>" . eshell)
  :custom
  (eshell-banner-message "                |
              \\ _ /
            -= (_) =-
              /   \\
_\\/_            |      o/      _\\/_
//o\\                  /|       /o\\\\
  |,__________________/_\\_______,|
  | '-Welcome to the Caribbean-' |\n"))

(use-package elec-pair
  :hook (prog-mode . electric-pair-mode))

(use-package elfeed
  :ensure t
  :bind
  ("C-c r" . elfeed)
  :custom
  (elfeed-use-curl t)
  (elfeed-search-title-max-width 100)
  (elfeed-search-title-min-width 100)
  (elfeed-db-directory "~/.cache/elfeed")
  (elfeed-search-filter "@1-month-ago +unread")
  (elfeed-feeds '(("https://reddit.com/r/emacs.rss" emacs)
                  ("https://reddit.com/r/unixporn.rss" linux)
                  ("https://reddit.com/r/openbsd.rss" linux openbsd)
                  ("http://feeds.feedburner.com/crunchyroll/rss/anime" anime)
                  ("https://sachachua.com/blog/category/emacs-news/feed" emacs news))))

(use-package eglot
  :ensure t
  :bind
  ("C-c l r" . eglot-rename)
  ("C-c l f" . eglot-format)
  ("C-c l g" . eglot-reconnect)
  ("C-c l h" . display-local-help)
  ("C-c l k" . eglot-shutdown-all)
  ("C-c l a" . eglot-code-actions)
  ([remap display-local-help] . nil)
  ("C-c l d" . eglot-find-declaration)
  ("C-c l t" . eglot-find-typeDefinition)
  ("C-c l i" . eglot-find-implementation)
  ("C-c l q" . eglot-code-action-quickfix)
  ("C-c l o" . eglot-code-action-organize-imports))

(use-package erc
  :bind
  ("C-c i" . erc-tls)
  :custom
  (erc-nick "josegpt")
  (erc-auto-query 'bury)
  (erc-track-shorten-start 8)
  (erc-kill-buffer-on-part t)
  (erc-server "irc.us.libera.chat")
  (erc-user-full-name "Jose G Perez Taveras")
  (erc-autojoin-channels-alist '(("irc.libera.chat" "#emacs" "#systemcrafters"))))

(use-package emacs
  :config
  (add-to-list 'default-frame-alist '(alpha . (85 . 85)))
  (set-frame-parameter (selected-frame) 'alpha '(85 . 85))
  (set-face-attribute 'default nil :family "Iosevka" :height 140)
  :custom
  (tab-width 2)
  (fill-column 72)
  (truncate-lines t)
  (indent-tabs-mode nil)
  (ring-bell-function 'ignore)
  (initial-major-mode #'fundamental-mode)
  ;; don't clutter up directories with files~
  (backup-directory-alist `((".*" . ,temporary-file-directory)))
  ;; don't clutter with #files either
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t))))

(use-package epg-config
  :custom
  (epg-pinentry-mode 'loopback))

(use-package eww
  :custom
  (eww-auto-rename-buffer t)
  (eww-header-line-format nil))

(use-package exwm
  :ensure t
  :init (exwm-enable)
  :bind
  ("C-q" . exwm-input-send-next-key)
  :custom
  (exwm-workspace-number 1)
  (exwm-workspace-warp-cursor t)
  (exwm-input-prefix-keys '(?\C-x
                            ?\C-c
                            ?\C-u
                            ?\C-h
                            ?\C-g
                            ?\M-x
                            ?\M-:
                            ?\M-!
                            s-return
                            ?\s-0
                            ?\s-1
                            ?\s-2
                            ?\s-3
                            ?\s-o
                            ?\s-b
                            ?\s-f
                            ?\s-k
                            ?\s-p
                            ?\s-n
                            ?\s-a))
  (exwm-input-global-keys
   `(([?\s-w] . exwm-workspace-switch)
     ([?\C-c ?\C-j] . exwm-reset)
     ([?\s-&] . (lambda (command)
                  (interactive (list (read-shell-command "$ ")))
                  (start-process-shell-command command nil command)))))
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
     ([?\M-p] . [M-up])
     ([?\M-n] . [M-down])
     ([?\M-<] . [home])
     ([?\M->] . [end])
     ([?\C-/] . [?\C-z])
     ([?\C-w] . [?\C-x])
     ([?\M-w] . [?\C-c])
     ([?\C-y] . [?\C-v])
     ([?\C-s] . [?\C-g])
     ([?\C-r] . [C-S-g])
     ([?\C-t] . [?\C-t])
     ([?\C-j] . [?\C-k])
     ([?\C-d] . [delete])
     ([?\C-c ?r] . [?\C-r])
     ([?\C-c ?s] . [?\C-f])
     ([?\C-c ?f] . [?\C-l])
     ([?\C-c ?h] . [?\C-a])
     ([?\C-c ?k] . [?\C-w])
     ([?\C-c ?/] . [C-S-z])
     ([?\M-@] . [C-S-right])
     ([?\C-c ?g] . [escape])
     ([?\C-\M-b] . [M-left])
     ([?\C-\M-f] . [M-right])
     ([?\C-k] . [C-S-end ?\C-x])
     ([?\M-d] . [C-S-right ?\C-x])
     ([M-backspace] . [C-S-left ?\C-x]))))

(use-package flymake
  :bind (:map flymake-mode-map
              ("M-g p" . flymake-goto-prev-error)
              ("M-g n" . flymake-goto-next-error)
              ("M-g M-p" . flymake-goto-prev-error)
              ("M-g M-n" . flymake-goto-next-error)))

(use-package frame
  :custom
  (blink-cursor-mode nil))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'")

(use-package html-mode
  :mode "\\.\\(html?\\|ejs\\)\\'")

(use-package hl-line
  :config
  (global-hl-line-mode))

(use-package icomplete
  :config
  (fido-mode)
  :custom
  (icomplete-separator " · ")
  (icomplete-compute-delay 0.0)
  (icomplete-prospects-height 1)
  (icomplete-delay-completions-threshold 0.0))

(use-package imenu
  :bind
  ("C-." . imenu)
  :custom
  (imenu-auto-rescan t))

(use-package js-mode
  :mode "\\.js\\'"
  :custom
  (js-indent-level 2))

(use-package ledger-mode
  :ensure t
  :mode "\\.\\(ledger\\|dat\\)\\'"
  :bind
  ("C-M-i" . completion-at-point)
  :custom
  (ledger-complete-in-steps t)
  (ledger-clear-whole-transactions t)
  (ledger-reports '(("bal" "%(binary) -f %(ledger-file) bal")
                    ("reg" "%(binary) -f %(ledger-file) reg")
                    ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
                    ("account" "%(binary) -f %(ledger-file) reg %(account)")
                    ("net worth" "%(binary) -f %(ledger-file) bal ^assets ^liabilities")
                    ("cash flow" "%(binary) -f %(ledger-file) bal ^income ^equity ^expenses"))))

(use-package server
  :unless (server-running-p)
  :config
  (server-start))

(use-package smtpmail
  :custom
  (smtpmail-smtp-service 465)
  (smtpmail-stream-type 'ssl)
  (smtpmail-smtp-user "josegpt27")
  (mail-user-agent 'mu4e-user-agent)
  (send-mail-function 'smtpmail-send-it)
  (user-full-name "Jose G Perez Taveras")
  (smtpmail-smtp-server "smtp.gmail.com")
  (user-mail-address "josegpt27@gmail.com")
  (smtpmail-auth-credentials "~/.authinfo.gpg")
  (smtpmail-default-smtp-server "smtp.gmail.com")
  (message-send-mail-function 'smtpmail-send-it))

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  :custom
  (magit-clone-default-directory "~/projects/")
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-hard t))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

(use-package minibuffer
  :custom
  (completion-ignore-case t)
  (completion-auto-select t)
  (completion-wrap-movement t)
  (completion-cycle-threshold 3)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(partial-completion substring)))

(use-package move-text
  :ensure t
  :bind
  ("M-p" . move-text-up)
  ("M-n" . move-text-down))

(use-package mu4e
  :bind
  ("C-c e" . mu4e)
  :custom
  (mu4e-confirm-quit nil)
  (mu4e-view-show-images t)
  (mu4e-view-show-addresses t)
  (mu4e-trash-folder "/Trash")
  (mu4e-drafts-folder "/Drafts")
  (mu4e-maildir "~/.cache/Mail")
  (mu4e-sent-folder "/Sent Mail")
  (message-kill-buffer-on-exit t)
  (mu4e-update-interval (* 60 30))
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-change-filenames-when-moving t)
  (mu4e-sent-messages-behavior 'delete)
  (mu4e-maildir-shortcuts
   '((:maildir "/INBOX" :key ?i)
     (:maildir "/Sent Mail" :key ?s)
     (:maildir "/Starred" :key ?f)
     (:maildir "/Spam" :key ?p)
     (:maildir "/Drafts" :key ?d)
     (:maildir "/Trash" :key ?t))))

(use-package otaku
  :bind
  ("C-c o s" . otaku-search-anime)
  ("C-c o r" . otaku-recent-anime-episodes))

(use-package paren
  :hook (prog-mode . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-context-when-offscreen t))

(use-package password-store
  :ensure t
  :bind
  ("C-c p e" . password-store-edit)
  ("C-c p w" . password-store-copy)
  ("C-c p c" . password-store-clear)
  ("C-c p i" . password-store-insert)
  ("C-c p r" . password-store-rename)
  ("C-c p k" . password-store-remove)
  ("C-c p g" . password-store-generate)
  ("C-c p f" . password-store-copy-field))

(use-package project
  :bind
  ("C-x p l" . eglot)
  ("C-x p m" . magit-project-status)
  :custom
  (project-switch-commands '((?l "Eglot LSP" eglot)
                             (?d "Dired" project-dired)
                             (?e "Eshell" project-eshell)
                             (?f "File" project-find-file)
                             (?c "Compile" project-compile)
                             (?m "Magit" magit-project-status)
                             (?b "Buffer" project-switch-to-buffer))))

(use-package pt-desktop
  :hook (exwm-update-title . pt-desktop-rename-workspace-buffer)
  :bind
  ("s-p" . pt-desktop-previous-workspace)
  ("s-n" . pt-desktop-next-workspace)
  ("<XF86AudioPlay>" . pt-desktop-play-pause-player)
  ("<XF86AudioStop>" . pt-desktop-stop-player)
  ("<XF86AudioNext>" . pt-desktop-next-player)
  ("<XF86AudioPrev>" . pt-desktop-previous-player)
  ("<XF86AudioRaiseVolume>" . pt-desktop-raise-volume)
  ("<XF86AudioLowerVolume>" . pt-desktop-lower-volume)
  ("<XF86AudioMute>" . pt-desktop-mute-volume)
  ("<M-XF86AudioRaiseVolume>" . pt-desktop-raise-mic-volume)
  ("<M-XF86AudioLowerVolume>" . pt-desktop-lower-mic-volume)
  ("<M-XF86AudioMute>" . pt-desktop-mute-mic-volume)
  ("<XF86AudioMicMute>" . pt-desktop-mute-mic-volume)
  ("<XF86MonBrightnessUp>" . pt-desktop-raise-brightness)
  ("<XF86MonBrightnessDown>" . pt-desktop-lower-brightness)
  ("s-a" . pt-desktop-powersettings))

(use-package shr
  :custom
  (shr-use-fonts nil))

(use-package simple
  :bind
  ("C-c d" . list-processes))

(use-package solar
  :custom
  (calendar-latitude 40.86)
  (calendar-longitude -74.16)
  (calendar-location-name "Clifton, NJ"))

(use-package sh-script
  :mode ("\\template\\'" . sh-mode))

(use-package subword
  :hook ((js-mode go-mode typescript-mode ledger-mode) . subword-mode))

(use-package time
  :custom
  (display-time-format "(%A) %B %d, %Y - %I:%M%P")
  :config
  (display-time-mode))

(use-package typescript-mode
  :ensure t
  :mode "\\.tsx?\\'"
  :custom
  (typescript-indent-level 2))

(use-package tooltip
  :custom
  (tooltip-mode nil))

(use-package webjump
  :bind
  ("C-c j" . webjump)
  :custom
  (webjump-sites '(("Gmail" . "mail.google.com")
                   ("Discord" . "discord.com/app")
                   ("Epic Games" . "epicgames.com")
                   ("Telegram" . "web.telegram.org")
                   ("WhatsApp" . "web.whatsapp.com")
                   ("Personal Website" . "josegpt.com")
                   ("Google Photos" . "photos.google.com")
                   ("Canal Packages" . "packages.ptserver.org")
                   ("Google Drive" . "drive.google.com/drive/my-drive")
                   ("Melpa" . [simple-query "melpa.org" "melpa.org/#/?q=" ""])
                   ("Amazon" . [simple-query "amazon.com" "amazon.com/s?k=" ""])
                   ("Reddit Sub" . [simple-query "reddit.com" "reddit.com/r/" ""])
                   ("Swappa" . [simple-query "swappa.com" "swappa.com/search?q=" ""])
                   ("Emacs Repository" . "http://git.savannah.gnu.org/cgit/emacs.git")
                   ("Github" . [simple-query "github.com" "github.com/search?q=" ""])
                   ("Ebay" . [simple-query "ebay.com" "ebay.com/sch/i.html?_nkw=" ""])
                   ("Twitch" . [simple-query "twitch.tv" "twitch.tv/search?term=" ""])
                   ("Reddit" . [simple-query "reddit.com" "reddit.com/search/?q=" ""])
                   ("Wikipedia" . [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
                   ("Gitlab" . [simple-query "gitlab.com" "gitlab.com/search?search=" ""])
                   ("AnimeFLV" . [simple-query "animeflv.net" "animeflv.net/browse?q=" ""])
                   ("Gitlab User & Repository" . [simple-query "gitlab.com" "gitlab.com/" ""])
                   ("Github User & Repository" . [simple-query "github.com" "github.com/" ""])
                   ("Youtube" . [simple-query "youtube.com" "youtube.com/results?search_query=" ""])
                   ("Crunchyroll" . [simple-query "crunchyroll.com" "crunchyroll.com/search?&q=" ""])
                   ("Elpa" . [simple-query "elpa.gnu.org/packages/" "elpa.gnu.org/packages/" ".html"])
                   ("Google" . [simple-query "google.com" "google.com/search?q=" "+-site:pinterest.com"])
                   ("Youtube Music" . [simple-query "music.youtube.com" "music.youtube.com/search?q=" ""])
                   ("Repology" . [simple-query "repology.org/projects/" "repology.org/projects/?search=" ""])
                   ("OpenBSD Ports" . [simple-query "openports.se" "https://openports.se/search.php?so=" ""]))))

(use-package whitespace
  :hook ((prog-mode ledger-mode conf-mode) . whitespace-mode)
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

(use-package man
  :bind
  ("C-c m" . man))

(use-package window
  :bind
  ("s-0" . delete-window)
  ("s-1" . delete-other-windows)
  ("s-2" . split-window-below)
  ("s-3" . split-window-right)
  ("s-o" . other-window)
  ("s-b" . previous-buffer)
  ("s-f" . next-buffer)
  ("s-k" . kill-current-buffer)
  :custom
  (display-buffer-alist '(("\\`\\*Async Shell Command\\*\\'"
                           (display-buffer-no-window))
                          ("\\*\\(Calc\\|Process List\\|Proced\\)\\*"
                           (display-buffer-reuse-mode-window display-buffer-in-side-window)
                           (window-height . 0.20)
                           (side . bottom)
                           (slot . -1))
                          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|compilation\\)\\*"
                           (display-buffer-reuse-mode-window display-buffer-in-side-window)
                           (window-height . 0.20)
                           (side . bottom)
                           (slot . 0))
                          ("\\*\\(envrc\\)\\*"
                           (display-buffer-reuse-mode-window display-buffer-in-side-window)
                           (window-height . 0.20)
                           (side . bottom)
                           (slot . 1))
                          ("\\*\\(Ledger.*\\|Woman.*\\|Man.*\\|Help.*\\|godoc.*\\|eldoc.*\\|Buffer List\\)\\*"
                           (display-buffer-reuse-mode-window display-buffer-in-side-window)
                           (window-width . 0.45)
                           (side . right)
                           (slot . -1)))))

(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'")

(use-package vc
  :custom
  (vc-follow-symlinks t))

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode))

;;; init.el ends here
