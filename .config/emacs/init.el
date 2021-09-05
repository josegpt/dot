;;   ___ _ __ ___   __ _  ___ ___
;;  / _ \ '_ ` _ \ / _` |/ __/ __|
;; |  __/ | | | | | (_| | (__\__ \
;;  \___|_| |_| |_|\__,_|\___|___/
;; ============================================================
;; Straight
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

;;; profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
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

(use-package autorevert
  :init (global-auto-revert-mode t))

(use-package corfu
  :custom
  (corfu-cycle t)
  :hook ((prog-mode shell-mode eshell-mode) . corfu-mode)
  :init
  (corfu-global-mode))

(use-package diminish)

(use-package display-line-numbers
  :straight (:type built-in)
  :hook (prog-mode . display-line-numbers-mode)
  :custom
  (display-line-numbers-type 'relative)
  (display-line-numbers-current-absolute t))

(use-package erc
  :straight (:type built-in)
  :custom
  (erc-server "irc.us.libera.chat")
  (erc-nick "josegpt")
  (erc-user-full-name "Jose G Perez Taveras")
  (erc-track-shorten-start 8)
  (erc-kill-buffer-on-part t)
  (erc-auto-query 'bury)
  (erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs"))))

(use-package emacs
  :custom
  ;; tabs mode
  (indent-tabs-mode nil)
  (tab-width 2)
  ;; bell
  (ring-bell-function 'ignore)
  ;; don't clutter up directories with files~
  (backup-directory-alist
   `((".*" . ,temporary-file-directory)))
  ;; don't clutter with #files either
  (auto-save-file-name-transforms
   `((".*" ,temporary-file-directory t))))

(use-package eshell
  :straight (:type built-in)
  :bind
  ("<s-return>" . eshell))

(use-package frame
  :straight (:type built-in)
  :config
  (blink-cursor-mode 0))

(use-package magit
  :bind
  ("C-x g" . magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package marginalia
  :after vertico
  :init (marginalia-mode))

(use-package modus-themes
  :config
  (load-theme 'modus-operandi t))

(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package paren
  :straight (:type built-in)
  :hook (prog-mode . show-paren-mode))

(use-package projectile
  :bind-keymap
  ("C-x p" . projectile-command-map)
  :custom
  (projectile-project-search-path '("~/projects")))

(use-package pinentry
  :init (pinentry-start)
  :after eshell
  :custom
  (epg-pinentry-mode 'loopback))

(use-package rainbow-mode
  :diminish
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package tooltip
  :straight (:type built-in)
  :custom
  (tooltip-mode nil))

(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-cycle t))

(use-package webjump
  :straight (:type built-in)
  :bind
  ("s-j" . webjump)
  :custom
  (webjump-sites '(("Gmail" . "mail.google.com")
                   ("Discord" . "discord.com/app")
                   ("Telegram" . "web.telegram.org")
                   ("WhatsApp" . "web.whatsapp.com")
                   ("Melpa" . [simple-query "melpa.org" "melpa.org/#/?q=" ""])
                   ("Github" . [simple-query "github.com" "github.com/search?q=" ""])
                   ("Twitch" . [simple-query "twitch.tv" "twitch.tv/search?term=" ""])
                   ("Reddit" . [simple-query "reddit.com" "reddit.com/search/?q=" ""])
                   ("Google" . [simple-query "google.com" "www.google.com/search?q=" ""])
                   ("AnimeFLV" . [simple-query "animeflv.net" "animeflv.net/browse?q=" ""])
                   ("Youtube" . [simple-query "youtube.com" "youtube.com/results?search_query=" ""])
                   ("Crunchyroll" . [simple-query "crunchyroll.com" "crunchyroll.com/search?&q=" ""]))))

(use-package whitespace
  :straight (:type built-in)
  :diminish
  :bind
  ("C-c w" . whitespace-mode)
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
  :diminish
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.5))

(use-package yasnippet
  :diminish (yas-minor-mode)
  :init (yas-global-mode))

;; ============================================================
;; Language Configs
;; ============================================================

(use-package dockerfile-mode
  :mode
  ("\\Dockerfile\\'" . dockerfile-mode))

(use-package eldoc
  :straight (:type built-in)
  :diminish
  :hook ((emacs-lisp-mode lisp-interaction-mode) . eldoc-mode))

(use-package go-mode
  :mode
  ("\\.go" . go-mode))

(use-package js
  :straight (:type built-in)
  :mode
  ("\\.js\\'" . js-mode)
  :custom
  (js-indent-level 2))

(use-package markdown-mode
  :mode
  ("\\.md\\'" . markdown-mode))

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
  :if (string= system-name "josegpt-desktop")
  :after exwm
  :hook
  (exwm-randr-screen-change . (lambda ()
                                (start-process-shell-command
                                 "xrandr" nil "xrandr --setmonitor HDMI-1-1 1080/286x2160/572+1380+0 HDMI-1")
                                (start-process-shell-command
                                 "xrandr" nil "xrandr --setmonitor HDMI-1-2 1380/368x1080/286+0+1080 none")
                                (start-process-shell-command
                                 "xrandr" nil "xrandr --setmonitor HDMI-1-3 1380/368x1080/286+0+0 none")
                                (start-process-shell-command
                                 "xrandr" nil "xrandr --setmonitor HDMI-1-4 1380/368x1080/286+2460+0 none")
                                (start-process-shell-command
                                 "xrandr" nil "xrandr --setmonitor HDMI-1-5 1380/368x1080/286+2460+1080 none")
                                ))
  :config
  (exwm-randr-enable)
  :custom
  (exwm-randr-workspace-monitor-plist '(0 "HDMI-1-1" 1 "HDMI-1-2" 2 "HDMI-1-3" 3 "HDMI-1-4" 4 "HDMI-1-5")))

(use-package exwm
  :init (exwm-enable)
  :if (eq window-system 'x)
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
                            ("Firefox" (exwm-workspace-move-window 4))
                            ("mpv" (exwm-workspace-move-window 3)))))
  :bind
  (:map exwm-mode-map
        ("C-q" . exwm-input-send-next-key))
  :custom
  (exwm-workspace-number 5)
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
     ?\s-q
     ?\s-j))
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
     ([?\C-k] . [S-end delete])
     ([M-backspace] . [C-backspace])
     ([?\M-d] . [C-S-right delete]))))
