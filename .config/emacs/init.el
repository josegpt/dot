;;; init.el --- My init file. -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jose G Perez Taveras <josegpt27@gmail.com>

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;;; Code:

(unless (package-installed-p 'setup)
  (package-install 'setup))

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

(setup auth-source-pass
  (:if-package password-store)
  (auth-source-pass-enable))

(setup autorevert
  (global-auto-revert-mode t))

(setup bookmark
  (:global "s-l" #'bookmark-jump))

(setup corfu
  (:option corfu-cycle t)
  (:hook-into prog-mode
              shell-mode
              eshell-mode)
  (corfu-global-mode))

(setup daemons
  (:global "C-c d" #'daemons))

(setup dired
  (:global "s-d" #'dired))

(setup direnv
  (direnv-mode))

(setup display-line-numbers
  (:hook-into prog-mode)
  (:option display-line-numbers-type 'relative
           display-line-numbers-current-absolute t))

;; (setup eglot
;;   (:with-hook eglot-ensure
;;     (:hook js-mode
;;            sh-mode
;;            go-mode
;;            elixir-mode))
;;   (:global "C-c e e" #'eglot)
;;   (:with-map eglot-mode-map
;;     (:bind "C-c e h" #'eldoc
;;            "C-c e r" #'eglot-rename
;;            "C-c e f" #'eldoc-format-buffer
;;            "C-c e o" #'eglot-code-action-organize-imports)))

(setup elfeed
  (:require pt-elfeed)
  (:global "s-r" #'elfeed)
  (:with-map elfeed-search-mode-map
    (:bind "w" #'pt-elfeed-play-youtube-link))
  (:option elfeed-use-curl t
           elfeed-db-directory "~/.cache/elfeed"
           elfeed-search-title-max-width 100
           elfeed-search-title-min-width 100
           elfeed-feeds '(("https://reddit.com/r/emacs.rss" emacs)
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

(setup erc
  (:option erc-server "irc.us.libera.chat"
	         erc-nick "josegpt"
	         erc-user-full-name "Jose G Perez Taveras"
	         erc-track-shorten-start 8
	         erc-kill-buffer-on-part t
	         erc-auto-query 'bury
	         erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs"))))

(setup emacs
  (:option indent-tabs-mode nil
           tab-width 2
           ring-bell-function 'ignore
           ;; don't clutter up directories with files~
           backup-directory-alist
           `((".*" . ,temporary-file-directory))
           ;; don't clutter with #files either
           auto-save-file-name-transforms
           `((".*" ,temporary-file-directory t)))
  (load-theme 'modus-operandi t)
  (server-mode))

(setup eshell
  (:global "<s-return>" #'eshell))

(setup flymake
  (:hook-into prog-mode))

(setup frame
  (:option blink-cursor-mode 0))

(setup files
  (:global "s-f" #'find-file
           "s-s" #'save-buffer))

(setup icomplete
  (:option icomplete-compute-delay 0.0
           icomplete-delay-completions-threshold 200)
  (icomplete-mode)
  (fido-vertical-mode))

(setup keycast
  (:option keycast-separator-width 1
           keycast-remove-tail-elements nil)
  (keycast-mode))

(setup magit
  (:global "s-g" #'magit-status)
  (:option magit-clone-default-directory "~/projects/"
           magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(setup minibuffer
  (:option completion-ignore-case t
           read-buffer-completion-ignore-case t
           read-file-name-completion-ignore-case t
           completion-styles '(partial-completion substring)))

(setup move-text
  (:global "M-p" #'move-text-up
           "M-n" #'move-text-down))

(setup mu4e
  (:require mu4e)
  (:needs "mu"
          "mbsync")
  (:load-from "/home/josegpt/.guix-extra-profiles/base/base/share/emacs/site-lisp/")
  (:global "s-m" #'mu4e)
  (:option mu4e-change-filenames-when-moving t
           mu4e-view-show-addresses t
           mu4e-update-interval (* 60 60)
           mu4e-get-mail-command "mbsync -a"
           mu4e-view-show-images t
           mu4e-sent-messages-behavior 'delete
           mu4e-confirm-quit nil
           message-kill-buffer-on-exit t
           mu4e-compose-dont-reply-to-self t
           mu4e-attachment-dir "~/Downloads"
           user-full-name "Jose G Perez Taveras"
           user-mail-address "josegpt27@gmail.com"
           mu4e-maildir "~/Mail"
           mu4e-sent-folder "/Sent Mail"
           mu4e-drafts-folder "/Drafts"
           mu4e-trash-folder "/Trash"
           mu4e-maildir-shortcuts
           '((:maildir "/INBOX" :key ?i)
             (:maildir "/Sent Mail" :key ?s)
             (:maildir "/Starred" :key ?r)
             (:maildir "/Spam" :key ?p)
             (:maildir "/Drafts" :key ?d)
             (:maildir "/Trash" :key ?t))
           ;; Send Emails
           ;; FIXME: Add authinfo.gpg
           mail-user-agent 'mu4e-user-agent
           message-send-mail-function 'smtpmail-send-it
           smtpmail-smtp-user "josegpt27"
           smtpmail-smtp-server "smtp.gmail.com"
           smtpmail-smtp-service 465
           smtpmail-stream-type 'ssl)
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  (mu4e))

(setup project)

(setup password-store
  (:needs "pass")
  (:global "C-c p e" #'password-store-edit
           "C-c p w" #'password-store-copy
           "C-c p c" #'password-store-clear
           "C-c p i" #'password-store-insert
           "C-c p r" #'password-store-rename
           "C-c p k" #'password-store-remove
           "C-c p g" #'password-store-generate
           "C-c p f" #'password-store-copy-field))

(setup paren
  (:with-hook prog-mode-hook
    (:hook show-paren-mode)))

(setup pinentry
  (:needs "pinentry-emacs")
  (:option epg-pinentry-mode 'loopback)
  (pinentry-start))

(setup pt-desktop
  (:require pt-desktop)
  (:needs "amixer")
  (:global "s-a" #'pt-desktop-powersettings
           "<XF86AudioRaiseVolume>" #'pt-desktop-audio-volume-increment
           "<XF86AudioLowerVolume>" #'pt-desktop-audio-volume-decrement
           "<XF86AudioMute>" #'pt-desktop-audio-mute-toggle
           "<s-XF86AudioRaiseVolume>" #'pt-desktop-audio-mic-volume-increment
           "<s-XF86AudioLowerVolume>" #'pt-desktop-audio-mic-volume-decrement
           "<XF86AudioMicMute>" #'pt-desktop-audio-mic-mute-toggle
           "<XF86MonBrightnessUp>" #'pt-desktop-brightness-increment
           "<XF86MonBrightnessDown>" #'pt-desktop-brightness-decrement))

(setup subword
  (global-subword-mode t))

(setup tooltip
  (:option tooltip-mode nil))

(setup webjump
  (:global "s-j" #'webjump)
  (:option webjump-sites '(("Gmail" . "mail.google.com")
                           ("Discord" . "discord.com/app")
                           ("Telegram" . "web.telegram.org")
                           ("WhatsApp" . "web.whatsapp.com")
                           ("Melpa" . [simple-query "melpa.org" "melpa.org/#/?q=" ""])
                           ("Google" . [simple-query "google.com" "google.com/search?q=" ""])
                           ("Github" . [simple-query "github.com" "github.com/search?q=" ""])
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

(setup whitespace
  (:global "s-SPC" #'whitespace-mode)
  (:option whitespace-style '(face
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

(setup which-key
  (:option which-key-idle-delay 0.3)
  (which-key-mode))

(setup window
  (:global "s-0" #'delete-window
           "s-1" #'delete-other-windows
           "s-2" #'split-window-below
           "s-3" #'split-window-right
           "s-o" #'other-window
           "s-c" #'kill-current-buffer
           "s-C" #'kill-buffer-and-window
           "s-b" #'switch-to-buffer
           "s-k" #'kill-buffer)
  (:option display-buffer-alist '(("\\`\\*Async Shell Command\\*\\'"
                                   (display-buffer-no-window))
                                  ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|compilation\\)\\*"
                                   (display-buffer-in-side-window)
                                   (window-height . 0.25)
                                   (side . bottom)
                                   (slot . 1))
                                  ("\\*Help.*"
                                   (display-buffer-in-side-window)
                                   (window-width . 0.35)
                                   (side . right)
                                   (slot . -1))
                                  ("\\*.*e?shell.*"
                                   (display-buffer-reuse-mode-window display-buffer-at-bottom)
                                   (window-height . 0.25)))))

(setup yasnippet
  (yas-global-mode))

(setup dockerfile-mode
  (:file-match "\\Dockerfile\\'"))

(setup eldoc
  (:hook-into emacs-lisp-mode lisp-interaction-mode))

(setup go-mode
  (:file-match "\\.go\\'"))

(setup js
  (:file-match "\\.js\\'")
  (:option js-indent-level 2))

(setup markdown-mode
  (:file-match "\\.md\\'"))

(setup nov
  (:file-match "\\.epub\\'")
  (:option nov-text-width 80))

(setup pdf-tools
  (:require pdf-tools)
  (:with-mode pdf-view-mode
    (:file-match "\\.pdf\\'")))

(setup yaml-mode
  (:file-match "\\.ylm\\'"))

(setup exwm-randr
  (:if-host "guts")
  (:if-feature exwm)
  (:with-hook exwm-randr-screen-change-hook
    (:hook pt-exwm-xrandr-config))
  (:option
   exwm-randr-workspace-monitor-plist '(0 "HDMI-1-1" 1 "HDMI-1-2" 2 "HDMI-1-3" 3 "HDMI-1-4" 4 "HDMI-1-5"))
  (exwm-randr-enable))

(setup exwm
  (:require pt-exwm)
  (:with-hook exwm-update-class-hook
    (:hook pt-exwm-rename-buffer-with-class-name))
  (:with-hook exwm-update-title-hook
    (:hook pt-exwm-custom-rename-buffer-with-title))
  (:with-hook exwm-manage-finish-hook
    (:hook pt-exwm-send-window-to-workspace))
  (:with-map exwm-mode-map
    (:bind "C-q" #'exwm-input-send-next-key))
  (:option exwm-workspace-number 5
           exwm-workspace-switch-create-limit 5
           exwm-workspace-warp-cursor t
           exwm-input-prefix-keys '(?\C-x
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
                                    s-return
                                    XF86AudioRaiseVolume
                                    XF86AudioLowerVolume
                                    s-XF86AudioRaiseVolume
                                    s-XF86AudioLowerVolume
                                    XF86AudioMute
                                    XF86AudioMicMute
                                    XF86MonBrightnessUp
                                    XF86MonBrightnessDown)
           exwm-input-global-keys `(([?\s-w ?q] . exwm-reset)
                                    ([?\s-w ?s] . exwm-workspace-switch)
                                    ([?\s-w ?r] . pt-exwm-run-app)
                                    ,@(mapcar (lambda (i)
                                                `(,(kbd (format "C-s-%d" (1+ i))) .
                                                  (lambda ()
                                                    (interactive)
                                                    (exwm-workspace-switch-create ,i))))
                                              (number-sequence 0 4)))
           exwm-input-simulation-keys '(([?\C-b] . [left])
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
                                        ([M-backspace] . [C-S-left C-x])))
  (exwm-enable))
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(setup)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
