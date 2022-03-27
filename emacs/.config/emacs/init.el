;;; init.el --- description -*- lexical-binding: t -*-
;;; Package.el

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'setup)
  (package-refresh-contents)
  (package-install 'setup))

;;; the default is 800 kilobytes. measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;;; include custom elisp
(add-to-list 'load-path
             (concat user-emacs-directory
                     (convert-standard-filename "lisp/")))

;; native-compile all Elisp files under a directory
(native-compile-async
 (concat user-emacs-directory
         (convert-standard-filename "lisp/")) 'recursively)

;;; Packages

(setup auth-source-pass
  (auth-source-pass-enable))

(setup autorevert
  (:option global-auto-revert-non-file-buffers nil)
  (global-auto-revert-mode))

(setup battery
  (:only-if (string= (system-name) "josegpt-laptop.lan"))
  (display-battery-mode))

(setup browse-url
  (:option browse-url-browser-function 'eww-browse-url
           browse-url-secondary-browser-function 'browse-url-default-browser
           browse-url-handlers
           '((".*\\([Ww]hatsapp\\|[Dd]iscord\\)" . browse-url-default-browser)
             (".*\\([Yy]outube\\|[Gg]oogle\\)" . browse-url-default-browser))))

(setup (:require canales)
  (:global "C-c c" #'canales-watch))

(setup compile
  (:option compilation-scroll-output t))

(setup css-mode
  (:file-match "\\.\\(css\\|less\\|sass\\|scss\\|styl\\)\\'")
  (:hook display-line-numbers-mode
         whitespace-mode))

(setup (:require display-sunrise-sunset)
  (:option calendar-latitude 40.86
           calendar-longitude -74.16
           calendar-location-name "Clifton, NJ")
  (display-sunrise-sunset-mode))

(setup (:package display-wttr)
  (:option display-wttr-format "2")
  (display-wttr-mode))

(setup dired
  (:option dired-dwim-target t
           dired-use-ls-dired nil
           dired-listing-switches "-alh"
           dired-kill-when-opening-new-dired-buffer t))

(setup (:package dockerfile-mode)
  (:file-match "\\Dockerfile\\'")
  (:hook display-line-numbers-mode
         whitespace-mode
         display-line-numbers-mode))

(setup display-line-numbers
  (:option display-line-numbers-type 'relative
           display-line-numbers-current-absolute t))

(setup emacs-lisp-mode
  (:file-match "\\.el\\'")
  (:hook electric-pair-mode
         show-paren-mode
         display-line-numbers-mode
         whitespace-mode))

(setup eshell
  (:global "s-<return>" #'eshell)
  (:option eshell-banner-message "                |
              \\ _ /
            -= (_) =-
              /   \\
_\\/_            |      o/      _\\/_
//o\\                  /|       /o\\\\
  |,__________________/_\\_______,|
  | '-Welcome to the Caribbean-' |\n"))

(setup erc
  (:global "C-c i" #'erc-tls)
  (:option erc-nick "josegpt"
           erc-auto-query 'bury
           erc-track-shorten-start 8
           erc-kill-buffer-on-part t
           erc-server "irc.us.libera.chat"
           erc-user-full-name "Jose G Perez Taveras"
           erc-autojoin-channels-alist
           '(("irc.libera.chat" "#openbsd" "#emacs" "#systemcrafters"))))

(setup emacs
  (:option tab-width 4
           fill-column 72
           cursor-type 'bar
           indent-tabs-mode nil
           ring-bell-function 'ignore
           initial-major-mode #'fundamental-mode
           ;; don't clutter up directories with files~
           backup-directory-alist `((".*" . ,temporary-file-directory))
           ;; don't clutter with #files either
           auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (add-to-list 'default-frame-alist '(alpha . (85 . 85)))
  (set-frame-parameter (selected-frame) 'alpha '(85 . 85))
  (if (string= (system-name) "josegpt-laptop.lan")
      (set-face-attribute 'default nil
                          :family "Iosevka"
                          :height 100
                          :weight 'light)
    (set-face-attribute 'default nil
                        :family "Iosevka"
                        :height 140
                        :weight 'light)))

(setup eww
  (:option eww-auto-rename-buffer t
           eww-header-line-format nil
           eww-search-prefix "https://duckduckgo.com/lite?q="))

(setup frame
  (:option blink-cursor-mode nil))

(setup (:package go-mode)
  (:file-match "\\.go\\'")
  (:option gofmt-command "goimports")
  (:local-set compile-command "go run ")
  (:local-hook before-save-hook #'gofmt-before-save)
  (:hook subword-mode
         electric-pair-mode
         show-paren-mode
         display-line-numbers-mode
         whitespace-mode))

(setup html-mode
  (:file-match "\\.\\(html?\\|ejs\\)\\'")
  (:hook show-paren-mode
         display-line-numbers-mode
         whitespace-mode))

(setup hl-line
  (global-hl-line-mode))

(setup icomplete
  (:option icomplete-separator " · "
           icomplete-compute-delay 0.0
           icomplete-prospects-height 1
           icomplete-delay-completions-threshold 0.0)
  (fido-mode))

(setup js-mode
  (:file-match "\\.js\\'")
  (:option js-indent-level 2)
  (:local-set compile-command "npm run ")
  (:hook subword-mode
         electric-pair-mode
         show-paren-mode
         whitespace-mode))

(setup (:package ledger-mode)
  (:file-match "\\.\\(ledger\\|dat\\)\\'")
  (:bind "C-M-i" #'completion-at-point)
  (:hook whitespace-mode)
  (:option ledger-complete-in-steps t
           ledger-clear-whole-transactions t
           ledger-reports
           '(("bal" "%(binary) -f %(ledger-file) bal")
             ("reg" "%(binary) -f %(ledger-file) reg")
             ("budget" "%(binary) -f %(ledger-file) bal --budget")
             ("account" "%(binary) -f %(ledger-file) reg %(account)")
             ("net worth" "%(binary) -f %(ledger-file) bal ^assets ^liabilities")
             ("cash flow" "%(binary) -f %(ledger-file) bal ^income ^equity ^expenses"))))

(setup (:package magit)
  (:global "C-x p m" #'magit-project-status)
  (:hook electric-pair-mode)
  (:option magit-clone-default-directory "~/projects/"
           magit-display-buffer-function
           #'magit-display-buffer-same-window-except-diff-v1
           magit-clone-url-format "git@%h:%n"
           magit-clone-name-alist '(("\\`\\(?:github:\\|gh:\\)?\\([^:]+\\)\\'" "github.com" "github.user")
                                    ("\\`\\(?:sourcehut:\\|sr:\\)?\\([^:]+\\)\\'" "git.sr.ht" "sourcehut.user"))))



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

(setup man
  (:global "C-c m" #'man))

(setup modus-themes
  (load-theme 'modus-vivendi t))

(setup show-paren-mode
  (:option show-paren-when-point-inside-paren t
           show-paren-context-when-offscreen t))

(setup sh-mode
  (:file-match "\\APKBUILD\\'")
  (:hook subword-mode
         electric-pair-mode
         show-paren-mode
         display-line-numbers-mode
         whitespace-mode))

(setup (:package project))

(setup (:package protobuf-mode)
  (:file-match "\\.proto\\'")
  (:hook subword-mode
         electric-pair-mode
         display-line-numbers-mode
         whitespace-mode))

(setup shr
  (:option shr-width 72
           shr-use-fonts nil))

(setup simple
  (:global "C-c s" #'list-processes
           "M-z" #'zap-to-char
           "M-Z" #'zap-up-to-char))

(setup smtpmail
  (:option smtpmail-smtp-service 465
           smtpmail-stream-type 'ssl
           message-signature "josegpt"
           smtpmail-smtp-user "josegpt27"
           mail-user-agent 'sendmail-user-agent
           send-mail-function 'smtpmail-send-it
           user-full-name "Jose G Perez Taveras"
           smtpmail-smtp-server "smtp.gmail.com"
           user-mail-address "josegpt27@gmail.com"
           smtpmail-auth-credentials "~/.authinfo.gpg"
           smtpmail-default-smtp-server "smtp.gmail.com"
           message-send-mail-function 'smtpmail-send-it))

(setup tab-bar
  (:option tab-bar-new-button-show nil
           tab-bar-close-button-show nil
           tab-bar-format '(tab-bar-format-global))
  (tab-bar-mode))

(setup time
  (:option display-time-format "%B %d %Y - %I:%M%P")
  (display-time-mode))

(setup tooltip
  (:option tooltip-mode nil))

(setup vc
  (:option vc-follow-symlinks t))

(setup webjump
  (:global "C-c j" #'webjump)
  (:option webjump-sites
           '(("Sourcehut" . "sr.ht")
             ("Jose G" . "josegpt.com")
             ("Gmail" . "mail.google.com")
             ("PTServer" . "ptserver.org")
             ("Git Sourcehut" . "git.sr.ht")
             ("Discord" . "discord.com/app")
             ("WhatsApp" . "web.whatsapp.com")
             ("Builds Sourcehut" . "builds.sr.ht")
             ("Google Photos" . "photos.google.com")
             ("Google Drive" . "drive.google.com/drive/my-drive")
             ("Melpa" . [simple-query "melpa.org"
                                      "melpa.org/#/?q=" ""])
             ("Wikipedia" . [simple-query "wikipedia.org"
                                          "wikipedia.org/wiki/" ""])
             ("AnimeFLV" . [simple-query "animeflv.net"
                                         "animeflv.net/browse?q=" ""])
             ("Elpa" . [simple-query "elpa.gnu.org/packages/"
                                     "elpa.gnu.org/packages/" ".html"])
             ("Github User & Repository" . [simple-query "github.com"
                                                         "github.com/" ""])
             ("Youtube Music" . [simple-query "music.youtube.com"
                                              "music.youtube.com/search?q=" ""])
             ("Youtube" . [simple-query "youtube.com"
                                        "youtube.com/results?search_query=" ""])
             ("Google" . [simple-query "google.com"
                                       "google.com/search?q=" "+-site:pinterest.com"])
             ("Repology" .
              "https://repology.org/projects/?search=&maintainer=josegpt27%40gmail.com"))))

(setup whitespace
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

(setup window
  (:global "s-0" #'delete-window
           "s-1" #'delete-other-windows
           "s-2" #'split-window-below
           "s-3" #'split-window-right
           "s-o" #'other-window
           "s-b" #'previous-buffer
           "s-f" #'next-buffer
           "s-k" #'kill-current-buffer)
  (:option display-buffer-alist
           '(("\\`\\*Async Shell Command\\*\\'"
              (display-buffer-no-window))
             ("\\*\\(Calc\\|Process List\\|Proced\\|Buffer List\\)\\*"
              (display-buffer-reuse-mode-window display-buffer-in-side-window)
              (window-height . 0.30)
              (side . bottom)
              (slot . 1))
             ("\\*\\(Backtrace\\|Warnings\\|compilation\\|Gofmt.*\\)\\*"
              (display-buffer-reuse-mode-window display-buffer-in-side-window)
              (window-width . 0.50)
              (side . right)
              (slot . -1))
             ("\\*\\(Help\\|Man.*\\|Woman.*\\|Occur\\|Ledger.*\\)\\*"
              (display-buffer-reuse-mode-window display-buffer-in-side-window)
              (window-width . 0.50)
              (side . right)
              (slot . 0)))))

(setup (:package yaml-mode)
  (:file-match "\\.ya?ml\\'")
  (:hook display-line-numbers-mode
         whitespace-mode))

(setup (:package yasnippet)
  (yas-global-mode))

;;; init.el ends here
