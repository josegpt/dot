;;; init.el --- description -*- lexical-binding: t -*-
;;; Straight Config

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

;; include custom elisp
(add-to-list 'load-path (concat user-emacs-directory
                                (convert-standard-filename "lisp/")))

;; native-compile all Elisp files under a directory
;; (native-compile-async (concat user-emacs-directory
;;                               (convert-standard-filename "lisp/")) 'recursively)

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

;;; Packages

(setup auth-source-pass
  (auth-source-pass-enable))

(setup autorevert
  (:option global-auto-revert-non-file-buffers nil)
  (global-auto-revert-mode))

(setup battery
  (:only-if (string= (system-name) "josegpt-laptop"))
  (display-battery-mode))

(setup bookmark
  (:global "C-c b" #'bookmark-jump))

(setup browse-url
  (:option browse-url-browser-function 'eww-browse-url
           browse-url-secondary-browser-function 'browse-url-default-browser
           browse-url-handlers '((".*\\(youtube\\|twitch\\|discord\\|whatsapp\\|telegram\\|google\\)" . browse-url-default-browser))))

(setup (:require canales)
  (:global "C-c c" #'canales-watch))

(setup compile
  (:option compilation-scroll-output t))

(setup (:package corfu)
  (:option corfu-cycle t)
  (:hook-into prog-mode
              shell-mode
              eshell-mode
              ledger-mode))

(setup css-mode
  (:file-match "\\.\\(css\\|less\\|sass\\|scss\\|styl\\)\\'"))

(setup dired
  (:option dired-dwim-target t
           dired-listing-switches "-alh"
           dired-kill-when-opening-new-dired-buffer t))

(setup (:package dockerfile-mode)
  (:file-match "\\Dockerfile\\'"))

(setup display-line-numbers
  (:option display-line-numbers-type 'relative
           display-line-numbers-current-absolute t)
  (:hook-into prog-mode
              html-mode
              conf-mode
              ledger-mode))

(setup elec-pair
  (electric-pair-mode))

(setup (:package elfeed)
  (:global "C-c r" #'elfeed)
  (:option elfeed-use-curl t
           elfeed-search-title-max-width 100
           elfeed-search-title-min-width 100
           elfeed-db-directory "~/.cache/elfeed"
           elfeed-search-filter "@1-month-ago +unread"
           elfeed-feeds '(("https://reddit.com/r/emacs.rss" emacs)
                          ("https://reddit.com/r/unixporn.rss" linux)
                          ("https://reddit.com/r/voidlinux.rss" linux void)
                          ("http://feeds.feedburner.com/crunchyroll/rss/anime" anime)
                          ("https://sachachua.com/blog/category/emacs-news/feed" emacs news))))

(setup (:package eglot)
  (:bind [remap display-local-help] nil
         "C-c e r" #'eglot-rename
         "C-c e f" #'eglot-format
         "C-c e g" #'eglot-reconnect
         "C-c e h" #'display-local-help
         "C-c e k" #'eglot-shutdown-all
         "C-c e a" #'eglot-code-actions
         "C-c e d" #'eglot-find-declaration
         "C-c e t" #'eglot-find-typeDefinition
         "C-c e i" #'eglot-find-implementation
         "C-c e q" #'eglot-code-action-quickfix
         "C-c e o" #'eglot-code-action-organize-imports))

(setup erc
  (:global "C-c i" #'erc-tls)
  (:option erc-nick "josegpt"
           erc-auto-query 'bury
           erc-track-shorten-start 8
           erc-kill-buffer-on-part t
           erc-server "irc.us.libera.chat"
           erc-user-full-name "Jose G Perez Taveras"
           erc-autojoin-channels-alist '(("irc.libera.chat" "#emacs" "#systemcrafters"))))

(setup emacs
  (:option tab-width 2
           fill-column 72
           truncate-lines t
           indent-tabs-mode nil
           ring-bell-function 'ignore
           initial-major-mode #'fundamental-mode
           ;; don't clutter up directories with files~
           backup-directory-alist `((".*" . ,temporary-file-directory))
           ;; don't clutter with #files either
           auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (load-theme 'modus-vivendi t)
  ;; (add-to-list 'default-frame-alist '(alpha . (85 . 85)))
  ;; (set-frame-parameter (selected-frame) 'alpha '(85 . 85))
  (set-face-attribute 'default nil :family "Iosevka" :height 140))

(setup eww
  (:option eww-auto-rename-buffer t
           eww-header-line-format nil))

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

(setup html-mode
  (:file-match "\\.\\(html?\\|ejs\\)\\'"))

(setup hl-line
  (global-hl-line-mode))

(setup icomplete
  (:option icomplete-separator " · "
           icomplete-compute-delay 0.0
           icomplete-prospects-height 1
           icomplete-delay-completions-threshold 0.0)
  (fido-mode))

(setup imenu
  (:option imenu-auto-rescan t)
  (:global "C-." #'imenu))

(setup js-mode
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
           mail-user-agent 'mu4e-user-agent
           send-mail-function 'smtpmail-send-it
           user-full-name "Jose G Perez Taveras"
           smtpmail-smtp-server "smtp.gmail.com"
           user-mail-address "josegpt27@gmail.com"
           smtpmail-auth-credentials "~/.authinfo.gpg"
           smtpmail-default-smtp-server "smtp.gmail.com"
           message-send-mail-function 'smtpmail-send-it))

(setup (:package magit)
  (:global "C-x g" #'magit-status)
  (:option magit-clone-default-directory "~/projects/"
           magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(setup (:package markdown-mode)
  (:file-match "\\.md\\'"))

(setup minibuffer
  (:option completion-ignore-case t
           completion-auto-select t
           completion-wrap-movement t
           completion-cycle-threshold 3
           read-buffer-completion-ignore-case t
           read-file-name-completion-ignore-case t
           completion-styles '(partial-completion substring)))

(setup (:package move-text)
  (:global "M-p" #'move-text-up
           "M-n" #'move-text-down))

;; FIXME: Activate when find a better way of deferring it.
;; (setup (:require mu4e)
;;   (:global "C-c e" #'mu4e)
;;   (:option mu4e-confirm-quit nil
;;            mu4e-view-show-images t
;;            mu4e-view-show-addresses t
;;            mu4e-trash-folder "/Trash"
;;            mu4e-drafts-folder "/Drafts"
;;            mu4e-maildir "~/.cache/Mail"
;;            mu4e-sent-folder "/Sent Mail"
;;            message-kill-buffer-on-exit t
;;            mu4e-update-interval (* 60 30)
;;            mu4e-get-mail-command "mbsync -a"
;;            mu4e-attachment-dir "~/Downloads"
;;            mu4e-compose-dont-reply-to-self t
;;            mu4e-change-filenames-when-moving t
;;            mu4e-sent-messages-behavior 'delete
;;            mu4e-maildir-shortcuts
;;            '((:maildir "/INBOX" :key ?i)
;;              (:maildir "/Sent Mail" :key ?s)
;;              (:maildir "/Starred" :key ?f)
;;              (:maildir "/Spam" :key ?p)
;;              (:maildir "/Drafts" :key ?d)
;;              (:maildir "/Trash" :key ?t))))

(setup (:require otaku)
  (:global "C-c o s" #'otaku-search-anime
           "C-c o r" #'otaku-recent-anime-episodes))

(setup paren
  (:with-mode show-paren-mode
    (:hook-into prog-mode))
  (:option show-paren-when-point-inside-paren t
           show-paren-context-when-offscreen t))

(setup (:package password-store)
  (:global "C-c p e" #'password-store-edit
           "C-c p w" #'password-store-copy
           "C-c p c" #'password-store-clear
           "C-c p i" #'password-store-insert
           "C-c p r" #'password-store-rename
           "C-c p k" #'password-store-remove
           "C-c p g" #'password-store-generate
           "C-c p f" #'password-store-copy-field))

(setup (:package pinentry)
  (:option epg-pinentry-mode 'loopback)
  (pinentry-start))

(setup project
  (:option project-switch-commands '((?l "Eglot LSP" eglot)
                                     (?d "Dired" project-dired)
                                     (?e "Eshell" project-eshell)
                                     (?f "File" project-find-file)
                                     (?c "Compile" project-compile)
                                     (?m "Magit" magit-project-status)
                                     (?b "Buffer" project-switch-to-buffer)))
  (:global "C-x p l" #'eglot
           "C-x p m" #'magit-project-status))

(setup proced
  (:option proced-auto-update-timer 1)
  (:global "C-c d" #'proced))

(setup shr
  (:option shr-use-fonts nil))

(setup simple
  (:global "C-c l" #'list-processes))

(setup solar
  (:option calendar-latitude 40.86
           calendar-longitude -74.16
           calendar-location-name "Clifton, NJ"))

(setup sh-mode
  (:file-match "\\template\\'"))

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
  (:global "C-c j" #'webjump)
  (:option webjump-sites '(("Gmail" . "mail.google.com")
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
                           ("Void Packages" . [simple-query "voidlinux.org/packages/" "voidlinux.org/packages/?arch=x86_64&q=" ""]))))

(setup whitespace
  (:hook-into prog-mode
              ledger-mode
              conf-mode)
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

(setup woman
  (:global "C-c m" #'woman))

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
  (:option display-buffer-alist `(("\\`\\*Async Shell Command\\*\\'"
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

(setup (:package vue-mode)
  (:file-match "\\.vue\\'"))

(setup vc
  (:option vc-follow-symlinks t))

(setup (:package yaml-mode)
  (:file-match "\\.ya?ml\\'"))

(setup (:package yasnippet)
  (yas-global-mode))

;;; init.el ends here
