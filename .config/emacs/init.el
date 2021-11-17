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

(add-hook 'emacs-startup-hook #'(lambda ()
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

(use-package weeb
  :straight nil
  :bind
  ("C-c a" . weeb-search-anime))

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

(use-package eshell
  :bind
  ("<s-return>" . eshell))

(use-package envrc
  :config
  (envrc-global-mode))

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
  (elfeed-feeds '(("https://reddit.com/r/guix.rss" linux)
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
  (erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs"))))

(use-package emacs
  :straight (:type built-in)
  :config
  (load-theme 'modus-vivendi t)
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

(use-package hl-line
  :straight (:type built-in)
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
  ("C-x g" . magit-status)
  :custom
  (magit-clone-default-directory "~/projects/")
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package marginalia
  :after minibuffer
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

(use-package nroff-mode
  :bind
  (:map nroff-mode-map
        ("C-c C-p" . nroff-pdf-view))
  :mode "\\.ms\\'")

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
  :after minibuffer
  :config
  (pinentry-start)
  :custom
  (epg-pinentry-mode 'loopback))

(use-package project
  :bind
  ("C-x p m" . magit-project-status)
  ("C-x p a" . envrc-allow)
  ("C-x p l" . eglot))

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

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

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
                   ("Youtube Music" . [simple-query "music.youtube.com" "music.youtube.com/search?q=" ""]))))


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
  :bind
  ("s-1" . delete-other-windows)
  ("s-2" . split-window-below)
  ("s-3" . split-window-right)
  ("s-o" . other-window)
  ("s-p" . previous-buffer)
  ("s-n" . next-buffer)
  ("s-0" . delete-window)
  ("s-k" . kill-current-buffer)
  ("s-K" . kill-buffer-and-window)
  :custom
  (display-buffer-alist '(("\\`\\*Async Shell Command\\*\\'"
                           (display-buffer-no-window))
                          ("\\*\\(Help.*\\|Ledger.*\\|Backtrace\\|Warnings\\|Compile-Log\\|compilation\\|envrc\\|.*e?shell\\)\\*"
                           (display-buffer-in-side-window)
                           (window-width . 0.35)
                           (side . right)
                           (slot . -1)))))

(use-package yasnippet
  :config
  (yas-global-mode))

(use-package dockerfile-mode
   :mode "\\Dockerfile\\'")

(use-package elm-mode
  :mode "\\.elm\\'"
  :hook
  (elm-mode . elm-indent-mode)
  (elm-mode . elm-format-on-save-mode))

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
  :magic ("%PDF" . pdf-view-mode)
  :mode
  ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :custom
  (typescript-indent-level 2))

(use-package yaml-mode
  :mode "\\.ya?lm\\'")

(use-package vue-mode
  :mode "\\.vue\\'")

(use-package exwm-workspace
  :straight nil
  :no-require t
  :bind
  ("s-b" . (lambda ()
             (interactive)
             (if (< 0 exwm-workspace-current-index)
                 (exwm-workspace-switch (1- exwm-workspace-current-index))
               (exwm-workspace-switch (1- (exwm-workspace--count))))))
  ("s-f" . (lambda ()
             (interactive)
             (if (> (1- (exwm-workspace--count)) exwm-workspace-current-index)
                 (exwm-workspace-switch (1+ exwm-workspace-current-index))
               (exwm-workspace-switch 0)))))

(use-package exwm
  :init (exwm-enable)
  :config
  (defun pt/run-command-with-message (cmmd)
    (message "%s" (shell-command-to-string cmmd)))
  :hook
  (exwm-update-class . (lambda ()
                         (exwm-workspace-rename-buffer exwm-class-name)))
  (exwm-update-title . (lambda ()
                         (exwm-workspace-rename-buffer exwm-title)))
  (exwm-manage-finish . (lambda ()
                          (pcase exwm-class-name
                            ("Firefox" (exwm-workspace-move-window 1)))))
  :bind
  ("<XF86AudioPlay>" . (lambda ()
                         (interactive)
                         (pt/run-command-with-message "playerctl play-pause")))
  ("<XF86AudioStop>" . (lambda ()
                         (interactive)
                         (pt/run-command-with-message "playerctl stop")))
  ("<XF86AudioNext>" . (lambda ()
                         (interactive)
                         (pt/run-command-with-message "playerctl next")))
  ("<XF86AudioPrev>" . (lambda ()
                         (interactive)
                         (pt/run-command-with-message "playerctl previous")))
  ("<XF86AudioRaiseVolume>" . (lambda ()
                                (interactive)
                                (pt/run-command-with-message "amixer set Master 10%+")))
  ("<XF86AudioLowerVolume>" . (lambda ()
                                (interactive)
                                (pt/run-command-with-message "amixer set Master 10%-")))
  ("<XF86AudioMute>" . (lambda ()
                         (interactive)
                         (pt/run-command-with-message "amixer set Master toggle")))
  ("<s-XF86AudioRaiseVolume>" . (lambda ()
                                  (interactive)
                                  (pt/run-command-with-message "amixer set Capture 10%+")))
  ("<s-XF86AudioLowerVolume>" . (lambda ()
                                  (interactive)
                                  (pt/run-command-with-message "amixer set Capture 10%-")))
  ("<s-XF86AudioMute>" . (lambda ()
                           (interactive)
                           (pt/run-command-with-message "amixer set Capture toggle")))
  ("<XF86AudioMicMute>" . (lambda ()
                            (interactive)
                            (pt/run-command-with-message "amixer set Capture toggle")))
  ("<XF86MonBrightnessUp>" . (lambda ()
                               (interactive)
                               (pt/run-command-with-message "xbacklight -inc 10%")))
  ("<XF86MonBrightnessDown>" . (lambda ()
                                 (interactive)
                                 (pt/run-command-with-message "xbacklight -dec 10%")))
  ("s-a" . (lambda ()
             (interactive)
             (let* ((cmmds '(("Reboot" . "rb")
                             ("Shutdown" . "sd")
                             ("Poweroff" . "po")))
                    (choice (assoc-string
                             (completing-read "Action: " cmmds  nil t)
                             cmmds t))
                    (cmmd (cdr choice)))
               (eshell-command cmmd))))
  (:map exwm-mode-map
        ("C-q" . exwm-input-send-next-key))
  :custom
  (exwm-workspace-number 2)
  (exwm-workspace-warp-cursor t)
  (exwm-input-prefix-keys
   '(?\C-x
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
     s-XF86AudioMute))
  (exwm-input-global-keys
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
               (number-sequence 0 (1- exwm-workspace-number)))))
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
     ([?\C-k] . [S-end delete])
     ([M-backspace] . [C-backspace])
     ([?\M-d] . [C-S-right delete]))))
