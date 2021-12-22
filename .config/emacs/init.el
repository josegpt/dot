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

;;; setup.el
(straight-use-package 'setup)

;;; the default is 800 kilobytes. measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "---> Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;; include custom elisp
(add-to-list 'load-path (concat user-emacs-directory
                                (convert-standard-filename "lisp/")))

;; native-compile all Elisp files under a directory
(native-compile-async (concat user-emacs-directory
                              (convert-standard-filename "lisp/")) 'recursively)

(require 'setup)

(setup-define :package
  (lambda (recipe)
    `(unless (straight-use-package ',recipe)
       ,(setup-quit)))
  :documentation
  "Install RECIPE with `straight-use-package'.
This macro can be used as HEAD, and will replace itself with the
first RECIPE's package."
  :repeatable t
  :shorthand (lambda (sexp)
               (let ((recipe (cadr sexp)))
                 (if (consp recipe)
                     (car recipe)
                   recipe))))

;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(setup auth-source-pass
  (auth-source-pass-enable))

(setup autorevert
  (:option global-auto-revert-non-file-buffers nil)
  (global-auto-revert-mode))

(setup battery
  (:only-if (string= (system-name) "josegpt-laptop"))
  (display-battery-mode))

(setup bookmark
  (:global "s-q" #'bookmark-jump))

(setup (:require canales)
  (:global "s-c c" #'canales-watch))

(setup compile
  (:option compilation-scroll-output t))

(setup (:package corfu)
  (:option corfu-cycle t)
  (:hook-into prog-mode
              shell-mode
              eshell-mode
              ledger-mode))

(setup css
  (:file-match "\\.\\(css\\|less\\|sass\\|scss\\|styl\\)\\'"))

(setup (:package diff-hl)
  (global-diff-hl-mode))

(setup dired
  (:option dired-listing-switches "-alh"
           dired-kill-when-opening-new-dired-buffer t))

(setup (:package dockerfile-mode)
  (:file-match "\\Dockerfile\\'"))

(setup display-line-numbers
  (:option display-line-numbers-type 'relative
           display-line-numbers-current-absolute t)
  (:hook-into prog-mode
              html-mode
              ledger-mode))

(setup (:package envrc)
  (envrc-global-mode))

(setup eshell
  (:global "s-<return>" #'eshell))

(setup (:package elfeed)
  (:global "s-r" #'elfeed)
  (:option elfeed-use-curl t
           elfeed-db-directory "~/.cache/elfeed"
           elfeed-search-title-max-width 100
           elfeed-search-title-min-width 100
           elfeed-feeds '(("https://reddit.com/r/emacs.rss" emacs)
                          ("https://reddit.com/r/unixporn.rss" linux)
                          ("https://reddit.com/r/gentoo.rss" linux gentoo)
                          ("http://feeds.feedburner.com/crunchyroll/rss/anime" anime)
                          ("https://sachachua.com/blog/category/emacs-news/feed" emacs news))))

(setup (:package eglot)
  (:bind [remap display-local-help] nil
         "C-c m" #'imenu
         "C-c e" #'eldoc
         "C-c r" #'eglot-rename
         "C-c f" #'eglot-format
         "C-c g" #'eglot-reconnect
         "C-c h" #'display-local-help
         "C-c k" #'eglot-shutdown-all
         "C-c a" #'eglot-code-actions
         "C-c d" #'eglot-find-declaration
         "C-c t" #'eglot-find-typeDefinition
         "C-c i" #'eglot-find-implementation
         "C-c q" #'eglot-code-action-quickfix
         "C-c o" #'eglot-code-action-organize-imports))

(setup erc
  (:global "s-i" #'erc-tls)
  (:option erc-server "irc.us.libera.chat"
           erc-nick "josegpt"
           erc-user-full-name "Jose G Perez Taveras"
           erc-track-shorten-start 8
           erc-kill-buffer-on-part t
           erc-auto-query 'bury
           erc-autojoin-channels-alist '(("irc.libera.chat" "#emacs" "#systemcrafters"))))

(setup emacs
  (:option tab-width 2
           truncate-lines t
           indent-tabs-mode nil
           ring-bell-function 'ignore
           initial-major-mode #'emacs-lisp-mode
           ;; don't clutter up directories with files~
           backup-directory-alist `((".*" . ,temporary-file-directory))
           ;; don't clutter with #files either
           auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (load-theme 'modus-vivendi t)
  (set-face-attribute 'mode-line-active nil :inherit 'mode-line)
  (set-face-attribute 'mode-line-inactive nil :inherit 'mode-line)
  (add-to-list 'default-frame-alist '(alpha . (85 . 85)))
  (set-frame-parameter (selected-frame) 'alpha '(85 . 85)))

(setup eww
  (:option eww-auto-rename-buffer t
           eww-header-line-format nil))

(setup (:package exwm)
  (:bind "C-q" #'exwm-input-send-next-key)
  (:option exwm-workspace-number 2
           exwm-workspace-warp-cursor t
           exwm-input-prefix-keys '(?\C-x
                                    ?\C-c
                                    ?\C-u
                                    ?\C-h
                                    ?\C-g
                                    ?\M-x
                                    ?\M-:
                                    ?\M-!
                                    ?\s-c
                                    s-return
                                    ?\s-r
                                    ?\s-d
                                    ?\s-j
                                    ?\s-l
                                    ?\s-s
                                    ?\s-0
                                    ?\s-1
                                    ?\s-2
                                    ?\s-3
                                    ?\s-o
                                    ?\s-b
                                    ?\s-f
                                    ?\s-k
                                    ?\s-K
                                    ?\s-p
                                    ?\s-n
                                    ?\s-a
                                    ?\s-q
                                    XF86AudioPlay
                                    XF86AudioStop
                                    XF86AudioNext
                                    XF86AudioPrev
                                    XF86AudioRaiseVolume
                                    XF86AudioLowerVolume
                                    XF86AudioMute
                                    XF86AudioMicMute
                                    XF86MonBrightnessUp
                                    XF86MonBrightnessDown
                                    s-XF86AudioRaiseVolume
                                    s-XF86AudioLowerVolume
                                    s-XF86AudioMute)
           exwm-input-global-keys
           `(([?\s- ?w] . exwm-workspace-switch)
             ([?\s- ?r] . exwm-reset)
             ([?\s- ?&] . (lambda (command)
                            (interactive (list (read-shell-command "$ ")))
                            (start-process-shell-command command nil command))))
           exwm-input-simulation-keys
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
             ([M-backspace] . [C-S-left ?\C-x])))
  (exwm-enable))

(setup flymake
  (:bind "M-g p" #'flymake-goto-prev-error
         "M-g n" #'flymake-goto-next-error
         "M-g M-p" #'flymake-goto-prev-error
         "M-g M-n" #'flymake-goto-next-error))

(setup frame
  (:option blink-cursor-mode nil))

(setup (:package go-mode)
  (:file-match "\\.go\\'"))

(setup (:package haskell-mode)
  (:file-match "\\.hs\\'"))

(setup html
  (:file-match "\\.\\(html?\\|ejs\\)\\'"))

(setup hl-line
  (global-hl-line-mode))

(setup js
  (:file-match "\\.js\\'")
  (:option js-indent-level 2))

(setup (:package ledger-mode)
  (:file-match "\\.\\(ledger\\|dat\\)\\'")
  (:bind "C-M-i" #'completion-at-point)
  (:option ledger-complete-in-steps t
           ledger-clear-whole-transactions t
           ledger-reports '(("bal" "%(binary) -f %(ledger-file) bal")
                            ("reg" "%(binary) -f %(ledger-file) reg")
                            ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
                            ("account" "%(binary) -f %(ledger-file) reg %(account)")
                            ("net worth" "%(binary) -f %(ledger-file) bal ^assets ^liabilities")
                            ("cash flow" "%(binary) -f %(ledger-file) bal ^income ^equity ^expenses"))))

(setup smtpmail
  (:option smtpmail-smtp-service 465
           smtpmail-stream-type 'ssl
           smtpmail-smtp-user "josegpt27"
           mail-user-agent 'notmuch-user-agent
           send-mail-function 'smtpmail-send-it
           user-full-name "Jose G Perez Taveras"
           smtpmail-smtp-server "smtp.gmail.com"
           user-mail-address "josegpt27@gmail.com"
           smtpmail-auth-credentials "~/.authinfo.gpg"
           smtpmail-default-smtp-server "smtp.gmail.com"
           message-send-mail-function 'smtpmail-send-it))

(setup (:package magit)
  (:global "s-g" #'magit-status)
  (:option magit-clone-default-directory "~/projects/"
           magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(setup (:package marginalia)
  (marginalia-mode))

(setup (:package markdown-mode)
  (:file-match "\\.md\\'"))

(setup minibuffer
  (:option completion-ignore-case t
           completion-cycle-threshold 3
           read-buffer-completion-ignore-case t
           read-file-name-completion-ignore-case t
           completion-styles '(partial-completion substring)))

(setup (:package move-text)
  (:global "M-p" #'move-text-up
           "M-n" #'move-text-down))

(setup (:package notmuch)
  (:option notmuch-show-logo nil
           notmuch-search-oldest-first nil)
  (:global "s-m" #'notmuch))

(setup (:require otaku)
  (:global "s-c s" #'otaku-search-anime
           "s-c r" #'otaku-recent-anime-episodes))

(setup (:package orderless)
  (:option completion-styles '(orderless)
           completion-category-defaults nil
           completion-category-overrides '((file (styles . (partial-completion))))))

(setup paren
  (:with-mode show-paren-mode
    (:hook-into prog-mode))
  (:option show-paren-when-point-inside-paren t
           show-paren-context-when-offscreen t))

(setup (:package password-store)
  (:global "s-s e" #'password-store-edit
           "s-s w" #'password-store-copy
           "s-s c" #'password-store-clear
           "s-s i" #'password-store-insert
           "s-s r" #'password-store-rename
           "s-s k" #'password-store-remove
           "s-s g" #'password-store-generate
           "s-s f" #'password-store-copy-field))

(setup (:package pinentry)
  (:option epg-pinentry-mode 'loopback)
  (pinentry-start))

(setup (:package flatbuffers-mode)
  (:file-match "\\.fbs\\'"))

(setup project
  (:global "C-x p l" #'eglot
           "C-x p a" #'envrc-allow
           "C-x p m" #'magit-project-status))

(setup proced
  (:option proced-auto-update-timer 1)
  (:global "s-d" #'proced))

(setup (:require pt-desktop)
  (:if-feature exwm)
  (:with-hook exwm-update-title-hook
    (:hook pt-desktop-rename-workspace-buffer))
  (:with-hook exwm-manage-finish-hook
    (:hook pt-desktop-move-workspace-buffer))
  (:global "s-p" #'pt-desktop-previous-workspace
           "s-n" #'pt-desktop-next-workspace
           "<XF86AudioPlay>" #'pt-desktop-play-pause-player
           "<XF86AudioStop>" #'pt-desktop-stop-player
           "<XF86AudioNext>" #'pt-desktop-next-player
           "<XF86AudioPrev>" #'pt-desktop-previous-player
           "<XF86AudioRaiseVolume>" #'pt-desktop-raise-volume
           "<XF86AudioLowerVolume>" #'pt-desktop-lower-volume
           "<XF86AudioMute>" #'pt-desktop-mute-volume
           "<s-XF86AudioRaiseVolume>" #'pt-desktop-raise-mic-volume
           "<s-XF86AudioLowerVolume>" #'pt-desktop-lower-mic-volume
           "<s-XF86AudioMute>" #'pt-desktop-mute-mic-volume
           "<XF86AudioMicMute>" #'pt-desktop-mute-mic-volume
           "<XF86MonBrightnessUp>" #'pt-desktop-raise-brightness
           "<XF86MonBrightnessDown>" #'pt-desktop-lower-brightness
           "s-a" #'pt-desktop-powersettings))

(setup shr
  (:option shr-use-fonts nil))

(setup simple
  (:global "s-l" #'list-processes))

(setup subword
  (:hook-into js-mode
              go-mode
              haskell-mode
              typescript-mode
              ledger-mode))

(setup time
  (:option display-time-format "(%A) %B %d, %Y - %I:%M%P")
  (display-time-mode))

(setup (:package typescript-mode)
  (:file-match "\\.tsx?\\'")
  (:option typescript-indent-level 2))

(setup tooltip
  (:option tooltip-mode nil))

(setup webjump
  (:global "s-j" #'webjump)
  (:option webjump-sites '(("Gmail" . "mail.google.com")
                           ("Discord" . "discord.com/app")
                           ("Epic Games" . "epicgames.com")
                           ("Telegram" . "web.telegram.org")
                           ("WhatsApp" . "web.whatsapp.com")
                           ("Personal Website" . "josegpt.com")
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
                           ("AnimeFLV" . [simple-query "animeflv.net" "animeflv.net/browse?q=" ""])
                           ("Gitlab User & Repository" . [simple-query "gitlab.com" "gitlab.com/" ""])
                           ("Github User & Repository" . [simple-query "github.com" "github.com/" ""])
                           ("Youtube" . [simple-query "youtube.com" "youtube.com/results?search_query=" ""])
                           ("Crunchyroll" . [simple-query "crunchyroll.com" "crunchyroll.com/search?&q=" ""])
                           ("Elpa" . [simple-query "elpa.gnu.org/packages/" "elpa.gnu.org/packages/" ".html"])
                           ("Youtube Music" . [simple-query "music.youtube.com" "music.youtube.com/search?q=" ""])
                           ("Gentoo Packages" . [simple-query "packages.gentoo.org" "packages.gentoo.org/packages/search?q=" ""]))))

(setup whitespace
  (:hook-into prog-mode
              ledger-mode)
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

(setup (:package which-key)
  (:option which-key-idle-delay 1)
  (which-key-mode))

(setup woman
  (:global "s-w" #'woman))

(setup window
  (:global "s-0" #'delete-window
           "s-1" #'delete-other-windows
           "s-2" #'split-window-below
           "s-3" #'split-window-right
           "s-o" #'other-window
           "s-b" #'previous-buffer
           "s-f" #'next-buffer
           "s-k" #'kill-current-buffer
           "s-K" #'kill-buffer-and-window)
  (:option display-buffer-alist '(("\\*\\(Async Shell Command\\)\\*"
                                   (display-buffer-no-window))
                                  ("\\*\\(Calc\\|Process List\\|Proced\\|Buffer List\\)\\*"
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
                                  ("\\*\\(Ledger.*\\|Woman.*\\|Man.*\\|Help.*\\|godoc.*\\|eldoc.*\\)\\*"
                                   (display-buffer-reuse-mode-window display-buffer-in-side-window)
                                   (window-width . 0.45)
                                   (side . right)
                                   (slot . -1)))))

(setup (:package vertico)
  (:option vertico-cycle t)
  (vertico-mode))

(setup (:package vue-mode)
  (:file-match "\\.vue\\'"))

(setup (:package yaml-mode)
  (:file-match "\\.ya?lm\\'"))

(setup (:package yasnippet)
  (yas-global-mode))

;;; init.el ends here
