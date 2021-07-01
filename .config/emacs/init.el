;;   ___ _ __ ___   __ _  ___ ___
;;  / _ \ '_ ` _ \ / _` |/ __/ __|
;; |  __/ | | | | | (_| | (__\__ \
;;  \___|_| |_| |_|\__,_|\___|___/
;; ============================================================
;; Straight
;; ============================================================
;;;; Initialize straight
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
  ("<s-return>" . eshell)
  ("C-," . duplicate-line)
  ("C-c s" . powersettings)
  :config
  ;; auto refresh changed file
  (global-auto-revert-mode t)
  ;; no blinking
  (blink-cursor-mode 0)
  ;; disable menu bar
  (menu-bar-mode 0)
  ;; column indicator
  (global-display-fill-column-indicator-mode t)
  :custom
  ;; tabs mode
  (indent-tabs-mode nil)
  ;; bell
  (ring-bell-function 'ignore)
  ;; fill column
  (fill-column 80)
  ;; truncate lines
  (truncate-lines t)
  ;; disable statup messages
  (initial-scratch-message nil)
  (inhibit-startup-message t)
  ;; don't clutter up directories with files~
  (backup-directory-alist
   `((".*" . ,temporary-file-directory)))
  ;; don't clutter with #files either
  (auto-save-file-name-transforms
   `((".*" ,temporary-file-directory t))))

;; ============================================================
;; Packages
;; ============================================================

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
  (company-idle-delay 0)
  (company-minimum-prefix-length 1))

;;; diminish
(use-package diminish)

;;; eglot
(use-package eglot
  :hook ((js-mode ts-mode sh-mode) . eglot-ensure)
  :bind
  ("C-c e RET" . eglot)
  (:map eglot-mode-map
        ("C-c e h" . eldoc)
        ("C-c e r" . eglot-rename)
        ("C-c e f" . eldoc-format-buffer)
        ("C-c e o" . eglot-code-action-organize-imports)))

;;; expand region
(use-package expand-region
  :diminish
  :bind
  ("C-=" . er/expand-region))

;;; elfeed
(use-package elfeed
  :bind
  ("C-c r" . elfeed)
  (:map elfeed-search-mode-map
        ("g" . elfeed-update))
  :custom
  (elfeed-use-curl t)
  (elfeed-db-directory "~/.cache/elfeed")
  (elfeed-search-title-max-width 100)
  (elfeed-search-title-min-width 100)
  (elfeed-feeds '(("https://reddit.com/r/emacs.rss" emacs)
                  ("http://feeds.feedburner.com/crunchyroll/rss/anime" anime))))

;;; git gutter
(use-package git-gutter
  :diminish
  :hook (prog-mode . git-gutter-mode))

;;; magit
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;;; marginalia
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode)
  :custom
  (marginalia-margin-threshold 150))

;;; move text
(use-package move-text
  :bind
  (("M-p" . move-text-up)
   ("M-n" . move-text-down)))

;;; mu4e
;;   (use-package mu4e
;;     :straight nilv
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

;;; multiple cursors
(use-package multiple-cursors
  :bind
  ("C-c C->" . 'mc/mark-all-like-this)
  ("C->" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this))

;;; orderless
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

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

;;; pulseaudio control
(use-package pulseaudio-control
  :bind
  ("<XF86AudioLowerVolume>" . pulseaudio-control-decrease-volume)
  ("<XF86AudioRaiseVolume>" . pulseaudio-control-increase-volume)
  ("<XF86AudioMute>" . pulseaudio-control-toggle-current-sink-mute)
  ("<XF86AudioMicMute>" . pulseaudio-control-toggle-current-source-mute))

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

;;; theme
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-hard t))

;;; time
(use-package time
  :straight nil
  :config
  (display-time-mode t)
  :custom
  (display-time-default-load-average nil)
  (display-time-format "%a %B %d, %Y - %I:%M%P"))

;;; web jump
(use-package webjump
  :straight nil
  :bind
  ("C-c j" . webjump)
  :custom
  (webjump-sites '(("Gmail" . "mail.google.com")
                   ("Discord" . "discord.com/app")
                   ("Telegram" . "web.telegram.org")
                   ("WhatsApp" . "web.whatsapp.com")
                   ("Melpa" . [simple-query "melpa.org" "melpa.org/#/?q=" ""])
                   ("Github" . [simple-query "github.com" "github.com/search?q=" ""])
                   ("Reddit" . [simple-query "reddit.com" "reddit.com/search/?q=" ""])
                   ("Google" . [simple-query "google.com" "www.google.com/search?q=" ""])
                   ("AnimeFLV" . [simple-query "animeflv.net" "animeflv.net/browse?q=" ""])
                   ("Youtube" . [simple-query "youtube.com" "youtube.com/results?search_query=" ""])
                   ("Crunchyroll" . [simple-query "crunchyroll.com" "crunchyroll.com/search?&q=" ""]))))

;;; whitespace
(use-package whitespace
  :straight nil
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

;;; vertico
(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-cycle t))

;;; windmove
(use-package windmove
  :straight nil
  :bind
  ("s-f" . windmove-right)
  ("s-b" . windmove-left)
  ("s-p" . windmove-up)
  ("s-n" . windmove-down)
  ("s-F" . windmove-swap-states-right)
  ("s-B" . windmove-swap-states-left)
  ("s-P" . windmove-swap-states-up)
  ("s-N" . windmove-swapstates-down))

;;; yasnippet
(use-package yasnippet
  :diminish (yas-minor-mode)
  :hook ((prog-mode text-mode) . yas-minor-mode)
  :config
  (yas-reload-all))

;; ============================================================
;; Language Configs
;; ============================================================

;;; dockerfile mode
(use-package dockerfile-mode
  :mode
  ("\\Dockerfile\\'" . dockerfile-mode))

;;; eldoc
(use-package eldoc
  :straight nil
  :diminish
  :hook ((emacs-lisp-mode lisp-interaction-mode) . eldoc-mode))

;;; elixir mode
(use-package elixir-mode
  :mode
  ("\\.ex\\'" . elixir-mode)
  :hook (elixir-mode . (lambda ()
                        (add-hook 'before-save-hook 'elixir-format nil t))))

;;; elm mode
(use-package elm-mode
  :mode
  ("\\.elm\\'" . elm-mode)
  :hook (elm-mode . elm-format-on-save-mode))

;;; js-mode
(use-package js
  :straight nil
  :mode
  ("\\.js\\'" . js-mode)
  :custom
  (js-indent-level 2))

;;; markdown
(use-package markdown-mode
  :mode
  ("\\.md\\'" . markdown-mode))

;;; typescript mode
(use-package typescript-mode
  :mode ("\\.\\(ts\\|tsx\\)\\'" . typescript-mode)
  :config
  (setq typescript-indent-level 2))

;;; vue mode
(use-package vue-mode
  :mode ("\\.vue\\'" . vue-mode))

;;; yaml
(use-package yaml-mode
  :mode
  ("\\.ylm\\'" . yaml-mode))

;; ============================================================
;;   _____  ____      ___ __ ___
;;  / _ \ \/ /\ \ /\ / / '_ ` _ \
;; |  __/>  <  \ V  V /| | | | | |
;;  \___/_/\_\  \_/\_/ |_| |_| |_|
;; ============================================================
(use-package exwm-randr
  :straight nil
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
                           ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title))))))
  ;; send window to workspace
  (exwm-manage-finish . (lambda ()
                          (pcase exwm-class-name
                            ("Firefox" (exwm-workspace-move-window 3)))))
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
