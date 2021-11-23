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

;;; make setup use straight
(straight-use-package 'setup)

;;; the default is 800 kilobytes. measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(add-hook 'emacs-startup-hook
          #'(lambda ()
            (message "----> Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

(add-to-list 'load-path (concat user-emacs-directory
                                (convert-standard-filename "lisp/")))

;;;;;;;;;;;;;
;; Helpers ;;
;;;;;;;;;;;;;

(require 'setup)

(setup-define :if-host
  (lambda (hostname)
    `(unless (string= (system-name) ,hostname)
       ,(setup-quit)))
  :documentation "If HOSTNAME is not the current hostname, stop evaluating form.")

(setup-define :needs
    (lambda (executable)
      `(unless (executable-find ,executable)
         ,(setup-quit)))
  :documentation "If EXECUTABLE is not in the path, stop here."
  :repeatable 1)

(setup-define :straight
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

(setup-define :straight-when
  (lambda (recipe condition)
    `(if ,condition
         (straight-use-package ',recipe)
       ,(setup-quit)))
  :documentation
  "Install RECIPE with `straight-use-package' when CONDITION is met.
If CONDITION is false, stop evaluating the body.  This macro can
be used as HEAD, and will replace itself with the RECIPE's
package.  This macro is not repeatable."
  :repeatable nil
  :indent 1
  :shorthand (lambda (sexp)
               (let ((recipe (cadr sexp)))
                 (if (consp recipe) (car recipe) recipe))))

(defun nroff-pdf-view ()
  "Run pdf on this file."
  (interactive)
  (let* ((file (buffer-file-name))
         (file-pdf (concat (file-name-base) ".pdf"))
         (view-buff (get-buffer file-pdf)))
    (unless file
      (error "Buffer is not associated with any file"))
    (and (buffer-modified-p)
         (y-or-n-p (format "Save buffer %s first? " (buffer-name)))
         (save-buffer))
    (call-process-shell-command (format "groff -Tpdf -P-pa4 -ms %s > %s" (buffer-name) file-pdf))
    (unless view-buff
      (message "%s created." file-pdf)
      (display-buffer (find-file-noselect file-pdf)))))

;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(setup auth-source-pass
  (auth-source-pass-enable))

(setup autorevert
  (:option global-auto-revert-non-file-buffers nil)
  (global-auto-revert-mode))

(setup battery
  (:if-host "josegpt-laptop")
  (display-battery-mode))

(setup compile
  (:option compilation-scroll-output t))

(setup (:straight corfu)
  (:option corfu-cycle t)
  (:hook-into prog-mode
              shell-mode
              eshell-mode
              ledger-mode))

(setup css
  (:file-match "\\.\\(css\\|less\\|sass\\|scss\\|styl\\)\\'"))

(setup (:straight diff-hl)
  (global-diff-hl-mode))

(setup dired
  (:option dired-kill-when-opening-new-dired-buffer t))

(setup (:straight dockerfile-mode)
  (:file-match "\\Dockerfile\\'"))

(setup display-line-numbers
  (:option display-line-numbers-type 'relative
           display-line-numbers-current-absolute t)
  (:hook-into prog-mode
              html-mode
              ledger-mode))

(setup eshell
  (:global "s-<return>" eshell))

(setup (:straight envrc)
  (envrc-global-mode))

(setup (:straight elm-mode)
  (:file-match "\\.elm\\'")
  (:hook elm-indent-mode
         elm-format-on-save-mode))

(setup (:straight elfeed)
  (:global "C-c r" elfeed)
  (:option elfeed-use-curl t
           elfeed-db-directory "~/.cache/elfeed"
           elfeed-search-title-max-width 100
           elfeed-search-title-min-width 100
           elfeed-feeds '(("https://reddit.com/r/guix.rss" linux)
                          ("https://reddit.com/r/emacs.rss" emacs)
                          ("https://reddit.com/r/unixporn.rss" linux)
                          ("http://feeds.feedburner.com/crunchyroll/rss/anime" anime)
                          ("https://sachachua.com/blog/category/emacs-news/feed" emacs news))))

(setup erc
  (:global "C-c i" erc-tls)
  (:option erc-server "irc.us.libera.chat"
           erc-nick "josegpt"
           erc-user-full-name "Jose G Perez Taveras"
           erc-track-shorten-start 8
           erc-kill-buffer-on-part t
           erc-auto-query 'bury
           erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs"))))

(setup emacs
  (:option tab-width 2
           truncate-lines t
           indent-tabs-mode nil
           ring-bell-function 'ignore
           ;; don't clutter up directories with files~
           backup-directory-alist `((".*" . ,temporary-file-directory))
           ;; don't clutter with #files either
           auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (load-theme 'modus-vivendi t)
  (add-to-list 'default-frame-alist '(alpha . (85 . 85)))
  (set-frame-parameter (selected-frame) 'alpha '(85 . 85)))

(setup (:straight-when exwm (executable-find "startx"))
  (:bind "C-q" exwm-input-send-next-key)
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
                                    ?\s-i
                                    ?\s-1
                                    ?\s-2
                                    ?\s-3
                                    ?\s-o
                                    ?\s-p
                                    ?\s-n
                                    ?\s-0
                                    ?\s-k
                                    ?\s-K
                                    ?\s-b
                                    ?\s-f
                                    ?\s-a
                                    s-return
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
           `(([?\C-c ?\C-j] . exwm-reset)
             ([?\s-w] . exwm-workspace-switch)
             ([?\s-&] . (lambda (command)
                          (interactive (list (read-shell-command "$ ")))
                          (start-process-shell-command command nil command)))
             ,@(mapcar (lambda (i)
                         `(,(kbd (format "C-s-%d" (1+ i))) .
                           (lambda ()
                             (interactive)
                             (exwm-workspace-switch-create ,i))))
                       (number-sequence 0 (1- exwm-workspace-number))))
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

(setup frame
  (:option blink-cursor-mode nil))

(setup (:straight go-mode)
  (:file-match "\\.go\\'"))

(setup (:straight haskell-mode)
  (:file-match "\\.hs\\'"))

(setup html
  (:file-match "\\.\\(html?\\|ejs\\)\\'"))

(setup hl-line
  (global-hl-line-mode))

(setup (:straight vertico)
  (:option vertico-cycle t)
  (vertico-mode))

;; (use-package icomplete
;;   :config
;;   (icomplete-mode)
;;   (fido-vertical-mode)
;;   :custom
;;   (icomplete-compute-delay 0.0)
;;   (icomplete-delay-completions-threshold 200))

(setup js
  (:file-match "\\.js\\'")
  (:option js-indent-level 2))

(setup (:straight ledger-mode)
  (:file-match "\\.\\(ledger\\|dat\\)\\'")
  (:bind "C-M-i" completion-at-point)
  (:option ledger-complete-in-steps t
           ledger-clear-whole-transactions t
           ledger-reports '(("bal" "%(binary) -f %(ledger-file) bal")
                            ("reg" "%(binary) -f %(ledger-file) reg")
                            ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
                            ("account" "%(binary) -f %(ledger-file) reg %(account)")
                            ("net worth" "%(binary) -f %(ledger-file) bal ^assets ^liabilities")
                            ("cash flow" "%(binary) -f %(ledger-file) bal ^income ^equity ^expenses"))))

(setup (:straight-when magit (executable-find "git"))
  (:global "C-x g" magit-status)
  (:option magit-clone-default-directory "~/projects/"
           magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(setup (:straight marginalia)
  (:with-map minibuffer-local-map
    (:bind "M-A" marginalia-cycle))
  (marginalia-mode))

(setup (:straight markdown-mode)
  (:file-match "\\.md\\'"))

(setup minibuffer
  (:option completion-ignore-case t
           completion-cycle-threshold 3
           read-buffer-completion-ignore-case t
           read-file-name-completion-ignore-case t
           completion-styles '(partial-completion substring)))

(setup (:straight move-text)
  (:global "M-p" move-text-up
           "M-n" move-text-down))

(setup nroff-mode
  (:file-match "\\.ms\\'")
  (:bind "C-c C-p" nroff-pdf-view))

(setup (:straight nov)
  (:file-match "\\.epub\\'")
  (:option nov-text-width 80))

(setup (:require otaku)
  (:needs "mpv")
  (:global "C-c a" otaku-search-anime))

(setup (:straight orderless)
  (:option completion-styles '(orderless)
           completion-category-defaults nil
           completion-category-overrides '((file (styles . (partial-completion))))))

(setup paren
  (:with-mode show-paren-mode
    (:hook-into prog-mode))
  (:option show-paren-when-point-inside-paren t))

(setup (:straight-when password-store (executable-find "pass"))
  (:global "C-c p e"  password-store-edit
           "C-c p w" password-store-copy
           "C-c p c" password-store-clear
           "C-c p i" password-store-insert
           "C-c p r" password-store-rename
           "C-c p k" password-store-remove
           "C-c p g" password-store-generate
           "C-c p f" password-store-copy-field))

(setup (:require pt-desktop)
  (:with-hook exwm-update-title-hook
    (:hook pt-desktop-rename-workspace-buffer))
  (:with-hook exwm-manage-finish-hook
      (:hook pt-desktop-move-workspace-buffer))
  (:global "s-b" pt-desktop-previous-workspace
           "s-f" pt-desktop-next-workspace
           "<XF86AudioPlay>" pt-desktop-play-pause-player
           "<XF86AudioStop>" pt-desktop-stop-player
           "<XF86AudioNext>" pt-desktop-next-player
           "<XF86AudioPrev>" pt-desktop-previous-player
           "<XF86AudioRaiseVolume>" pt-desktop-raise-volume
           "<XF86AudioLowerVolume>" pt-desktop-lower-volume
           "<XF86AudioMute>" pt-desktop-mute-volume
           "<s-XF86AudioRaiseVolume>" pt-desktop-raise-mic-volume
           "<s-XF86AudioLowerVolume>" pt-desktop-lower-mic-volume
           "<s-XF86AudioMute>" pt-desktop-mute-mic-volume
           "<XF86AudioMicMute>" pt-desktop-mute-mic-volume
           "<XF86MonBrightnessUp>" pt-desktop-raise-brightness
           "<XF86MonBrightnessDown>" pt-desktop-lower-brightness
           "s-a" pt-desktop-powersettings))

(setup (:straight-when pinentry (executable-find "gpg"))
  (:option epg-pinentry-mode 'loopback)
  (pinentry-start))

(setup project
  (:global "C-x p m" magit-project-status
           "C-x p a" envrc-allow))

(setup proced
  (:global "C-c d" proced)
  (:option proced-auto-update-timer 1))

(setup (:straight prettier-js)
  (:hook-into html-mode
              js-mode
              typescript-mode
              css-mode
              markdown-mode
              vue-mode
              yaml-mode))

(setup (:straight rainbow-mode)
  (:hook-into prog-mode))

(setup subword
  (:hook-into js-mode
              go-mode
              elm-mode
              haskell-mode
              typescript-mode
              ledger-mode))

(setup time
  (:option display-time-format "(%A) %B %d, %Y - %I:%M%P")
  (display-time-mode))

(setup (:straight typescript-mode)
  (:file-match "\\.tsx?\\'")
  (:option typescript-indent-level 2))

(setup tooltip
  (:option tooltip-mode nil))

(setup webjump
  (:global "C-c j" webjump)
  (:option webjump-sites '(("Gmail" . "mail.google.com")
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
                           ("Youtube Music" . [simple-query "music.youtube.com" "music.youtube.com/search?q=" ""]))))

(setup whitespace
  (:hook-into prog-mode)
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

(setup (:straight which-key)
  (:option which-key-idle-delay 0.3)
  (which-key-mode))

(setup woman
  (:global "C-c w" woman))

(setup window
  (:global "s-0" delete-window
           "s-1" delete-other-windows
           "s-2" split-window-below
           "s-3" split-window-right
           "s-o" other-window
           "s-p" previous-buffer
           "s-n" next-buffer
           "s-k" kill-current-buffer
           "s-K" kill-buffer-and-window)
  (:option display-buffer-alist '(("\\`\\*Async Shell Command\\*\\'"
                                   (display-buffer-no-window))
                                  ("\\*\\(Ledger.*\\|Backtrace\\|Warnings\\|Compile-Log\\|compilation\\)\\*"
                                   (display-buffer-in-side-window)
                                   (window-width . 0.35)
                                   (side . right)
                                   (slot . -1))
                                  ("\\*\\(WoMan.*\\|Man.*\\|Help.*\\)\\*"
                                   (display-buffer-in-side-window)
                                   (window-width . 0.45)
                                   (side . left)
                                   (slot . -1))
                                  ("\\*\\(envrc\\)\\*"
                                   (display-buffer-in-side-window)
                                   (window-height . 0.25)
                                   (side . bottom)
                                   (slot . -1)))))

(setup (:straight vue-mode)
  (:file-match "\\.vue\\'"))

(setup (:straight yaml-mode)
  (:file-match "\\.ya?lm\\'"))

(setup (:straight yasnippet)
  (yas-global-mode))
;;; init.el ends here
