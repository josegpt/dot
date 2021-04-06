;;   ___ _ __ ___   __ _  ___ ___ 
;;  / _ \ '_ ` _ \ / _` |/ __/ __|
;; |  __/ | | | | | (_| | (__\__ \
;;  \___|_| |_| |_|\__,_|\___|___/

;; ============================================================
;; Startup
;; ============================================================
;;; debugging purposes
;; (setq use-package-verbose t)

;;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "---> Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; ============================================================
;; Config
;; ============================================================
(use-package emacs
  ;; enable emacs daemon
  :init (server-mode)
  :hook (prog-mode . display-line-numbers-mode)
  :bind
  ("C-," . duplicate-line)
  ("C-c s" . powersettings)
  :config
  ;; auto refresh changed file
  (global-auto-revert-mode t)
  ;; no blinking
  (blink-cursor-mode 0)
  ;; disable menu bar
  (menu-bar-mode -1)
  :custom
  ;; tabs mode
  (indent-tabs-mode nil)
  ;; relative line number
  (display-line-numbers-type (quote relative))
  ;; bell
  (ring-bell-function 'ignore)
  ;; disable statup messages
  (initial-scratch-message nil)
  (inhibit-startup-message t)
  ;; initial load elisp-mode
  (initial-major-mode 'emacs-lisp-mode)
  ;; don't clutter up directories with files~
  (backup-directory-alist
   `((".*" . ,temporary-file-directory)))
  ;; don't clutter with #files either
  (auto-save-file-name-transforms
   `((".*" ,temporary-file-directory t))))

;; ============================================================
;; Packages
;; ============================================================
;;; theme
(use-package nord-theme
  :config
  (load-theme 'nord t))

;;; auth source pass
(use-package auth-source-pass
  :after password-store
  :config
  (auth-source-pass-enable))

;;; company
(use-package company
  :diminish
  :hook (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;;; counsel
(use-package counsel
  :after ivy
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file))

;;; dired sidebar
(use-package dired-sidebar
  :bind
  ("C-c b" . dired-sidebar-toggle-sidebar))

;;; desktop env
(use-package desktop-environment
  :diminish
  :after exwm
  :init (desktop-environment-mode))

;;; expand region
(use-package expand-region
  :diminish
  :bind
  ("C-=" . er/expand-region))

;;; diminish
(use-package diminish)

;;; elfeed
(use-package elfeed
  :bind
  ("C-c f" . elfeed)
  (:map elfeed-search-mode-map
        ("g" . elfeed-update))
  :custom
  (elfeed-use-curl t)
  (elfeed-db-directory "~/.cache/elfeed")
  (elfeed-search-title-max-width 100)
  (elfeed-search-title-min-width 100)
  (elfeed-feeds '(("https://reddit.com/r/emacs.rss" emacs)
                  ("https://reddit.com/r/guix.rss" linux)
                  ("https://reddit.com/r/unixporn.rss" linux)
                  ("http://feeds.feedburner.com/crunchyroll/rss/anime" anime))))

;;; magit
(use-package magit
  :bind
  ("C-x g" . magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;;; move text
(use-package move-text
  :bind
  (("M-p" . move-text-up)
   ("M-n" . move-text-down)))

;;; mu4e
;;   (use-package mu4e
;;     :defer 10
;;     :bind
;;     ("C-c m" . mu4e)
;;     :custom
;;     (mu4e-change-filenames-when-moving t)
;;     (mu4e-view-show-addresses t)
;;     (mu4e-update-interval (* 10 60))
;;     (mu4e-get-mail-command "mbsync -a")
;;     ;; enable inline images
;;     (mu4e-view-show-images t)
;;     (mu4e-sent-messages-behavior 'delete)
;;     (mu4e-confirm-quit nil)
;;     (mu4e-attachment-dir "~/Downloads")
;;     (user-full-name "Jose G Perez Taveras")
;;     (user-mail-address "josegpt27@gmail.com")
;;     (mu4e-maildir "~/Mail")
;;     (mu4e-sent-folder "/Sent Mail")
;;     (mu4e-drafts-folder "/Drafts")
;;     (mu4e-trash-folder "/Trash")
;;     (mu4e-maildir-shortcuts
;;      '((:maildir "/INBOX" :key ?i)
;;        (:maildir "/Sent Mail" :key ?s)
;;        (:maildir "/Starred" :key ?r)
;;        (:maildir "/Spam" :key ?p)
;;        (:maildir "/Drafts" :key ?d)
;;        (:maildir "/Trash" :key ?t)))
;;     :config
;;     ;; init mu4e
;;     (mu4e t)
;;     ;; use imagemagick, if available
;;     (when (fboundp 'imagemagick-register-types)
;;       (imagemagick-register-types)))

;;; multiple Cursors
(use-package multiple-cursors
  :bind
  ("C->" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this)
  ("C-c C-<" . 'mc/mark-all-like-this))

;;; password-store
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

;;; pinentry
(use-package pinentry
  :init (pinentry-start)
  :custom
  ;; epg pinentry
  (epg-pinentry-mode 'loopback))

;;; rainbow mode
(use-package rainbow-mode
  :diminish
  :hook (prog-mode . rainbow-mode)
  :custom
  (rainbow-ansi-colors nil)
  (rainbow-x-colors nil))

;;; rainbow delimiters
(use-package rainbow-delimiters
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

;;; ivy
(use-package ivy
  :diminish
  :init (ivy-mode t)
  :bind
  ("C-s" . swiper)
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t))

;;; time
(use-package time
  :config
  (display-time-mode t)
  :custom
  (display-time-default-load-average nil)
  (display-time-format "%a %B %d, %Y - %I:%M%P"))

;;; web jump
(use-package webjump
  :bind
  ("C-c j" . webjump)
  :custom
  (webjump-sites '(("Gmail" . "mail.google.com")
                   ("Discord" . "discord.com/app")
                   ("Telegram" . "web.telegram.org")
                   ("WhatsApp" . "web.whatsapp.com")
                   ("Guix Packages" . "https://hpc.guix.info/browse")
                   ("Github" . [simple-query "github.com" "github.com/search?q=" ""])
                   ("Reddit" . [simple-query "reddit.com" "reddit.com/search/?q=" ""])
                   ("Google" . [simple-query "google.com" "www.google.com/search?q=" ""])
                   ("AnimeFLV" . [simple-query "animeflv.net" "animeflv.net/browse?q=" ""])
                   ("Youtube" . [simple-query "youtube.com" "youtube.com/results?search_query=" ""])
                   ("Crunchyroll" . [simple-query "crunchyroll.com" "crunchyroll.com/search?&q=" ""]))))

;;; whitespace
(use-package whitespace
  :diminish
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

;;; which key
(use-package which-key
  :diminish
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.5 "include delay to defer its execution"))

;;; yasnippet
(use-package yasnippet
  :diminish (yas-minor-mode)
  :hook ((prog-mode text-mode) . yas-minor-mode)
  :config
  (yas-reload-all))

;;;; ===> Language Config <===
;;; eldoc
(use-package eldoc
  :diminish
  :hook ((emacs-lisp-mode lisp-interaction-mode) . eldoc-mode))

;; ;;; elixir mode
;; (use-package elixir-mode
;;   :mode
;;   ("\\.ex\\'" . elixir-mode)
;;   :hook (elixir-mode . (lambda ()
;;                         (add-hook 'before-save-hook 'elixir-format nil t))))

;; ;;; elixir tooling
;; (use-package alchemist
;;   :diminish
;;   :after elixir-mode
;;   :custom
;;   (alchemist-hooks-test-on-save t))

;; ;;; elm mode
;; (use-package elm-mode
;;   :mode
;;   ("\\.elm\\'" . elm-mode)
;;   :hook (elm-mode . elm-format-on-save-mode))

;; ;;; markdown
;; (use-package markdown-mode
;;   :mode
;;   ("\\.md\\'" . markdown-mode))

;; ;;; prettier
;; (use-package prettier-js
;;   :diminish
;;   :hook ((js-mode web-mode css-mode) . prettier-js-mode))

;; ============================================================
;;   _____  ____      ___ __ ___  
;;  / _ \ \/ /\ \ /\ / / '_ ` _ \ 
;; |  __/>  <  \ V  V /| | | | | |
;;  \___/_/\_\  \_/\_/ |_| |_| |_|
;; ============================================================
(use-package exwm-randr
  :if (check-hostname "guts")
  :after exwm
  :hook
  (exwm-randr-screen-change . (lambda ()
                                (start-process-shell-command
                                 "xrandr" nil "xrandr --output DP-1-1 --primary --left-of DP-1-2-2-2 --output DP-1-2-1 --above DP-1-1 --rotate inverted --output DP-1-2-2-1 --right-of DP-1-2-1 --rotate inverted")))
  :config
  (exwm-randr-enable)
  :custom
  (exwm-randr-workspace-monitor-plist '(0 "DP-1-1" 1 "DP-1-2-1" 2 "DP-1-2-2-1" 3 "DP-1-2-2-2")))

(use-package exwm
  :init (exwm-enable)
  :hook
  ;; Make class name the buffer name
  (exwm-update-class . (lambda ()
			 (exwm-workspace-rename-buffer exwm-class-name)))
  (exwm-update-title . (lambda ()
			 (pcase exwm-class-name
			   ("Chromium-browser" (exwm-workspace-rename-buffer (format "Chromium: %s" exwm-title))))))
  ;; send window to workspace
  (exwm-manage-finish . (lambda ()
			  (pcase exwm-class-name
			    ("Chromium-browser" (exwm-workspace-move-window 1)))))
  :bind
  (:map exwm-mode-map
	("C-q" . exwm-input-send-next-key))
  :custom
  (exwm-workspace-number 4)
  (exwm-workspace-warp-cursor t)
  (exwm-input-prefix-keys
   '(?\C-x
     ?\C-c
     ?\C-u
     ?\C-h
     ?\C-g
     ?\M-x
     ?\M-:
     ?\M-!))
  (exwm-input-global-keys
   `(([?\s-r] . exwm-reset)
     ([?\s-w] . exwm-workspace-switch)
     ([?\s-&] . (lambda (command)
                  (interactive (list (read-shell-command "$ ")))
                  (start-process-shell-command command nil command)))
     ,@(mapcar (lambda (i)
                 `(,(kbd (format "s-%d" (1+ i))) .
                   (lambda ()
                     (interactive)
                     (exwm-workspace-switch-create ,i))))
               (number-sequence 0 3))))
  (exwm-input-simulation-keys
   '(([?\C-b] . [left])
     ([?\C-f] . [right])
     ([?\C-p] . [up])
     ([?\C-n] . [down])
     ([?\C-a] . [home])
     ([?\C-e] . [end])
     ([?\C-v] . [next])
     ([?\M-h] . [?\C-a])
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
     ([?\C-c ?f] . [?\C-l])
     ([?\C-c ?k] . [?\C-w])
     ([?\C-c ?g] . [escape])
     ([?\C-\M-b] . [M-left])
     ([?\C-\M-f] . [M-right])
     ([?\C-k] . [S-end delete])
     ([M-backspace] . [C-backspace])
     ([?\M-d] . [C-S-right delete]))))
