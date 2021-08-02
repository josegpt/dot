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
  ;; disbale tooltip
  (tooltip-mode -1)
  :custom
  ;; tabs mode
  (indent-tabs-mode nil)
  ;; bell
  (ring-bell-function 'ignore)
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

(use-package company
  :diminish
  :hook (prog-mode . company-mode)
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1))

(use-package diminish)

(use-package expand-region
  :diminish
  :bind
  ("C-=" . er/expand-region))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package marginalia
  :after vertico
  :init (marginalia-mode))

(use-package modus-themes
  :config
  (load-theme 'modus-vivendi t))

(use-package move-text
  :bind
  (("M-p" . move-text-up)
   ("M-n" . move-text-down)))

(use-package multiple-cursors
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package pinentry
  :init (pinentry-start)
  :custom
  (epg-pinentry-mode 'loopback))

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
                   ("Twitch" . [simple-query "twitch.tv" "twitch.tv/search?term=" ""])
                   ("Reddit" . [simple-query "reddit.com" "reddit.com/search/?q=" ""])
                   ("Google" . [simple-query "google.com" "www.google.com/search?q=" ""])
                   ("AnimeFLV" . [simple-query "animeflv.net" "animeflv.net/browse?q=" ""])
                   ("Youtube" . [simple-query "youtube.com" "youtube.com/results?search_query=" ""])
                   ("Crunchyroll" . [simple-query "crunchyroll.com" "crunchyroll.com/search?&q=" ""]))))

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

(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-cycle t))

(use-package which-key
  :diminish
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.5 "include delay to defer its execution"))

(use-package yasnippet
  :diminish (yas-minor-mode)
  :hook ((prog-mode text-mode) . yas-minor-mode)
  :config
  (yas-reload-all))

;; ============================================================
;; Language Configs
;; ============================================================

(use-package dockerfile-mode
  :mode
  ("\\Dockerfile\\'" . dockerfile-mode))

(use-package eldoc
  :straight nil
  :diminish
  :hook ((emacs-lisp-mode lisp-interaction-mode) . eldoc-mode))

(use-package elixir-mode
  :mode
  ("\\.ex\\'" . elixir-mode)
  :hook (elixir-mode . (lambda ()
                         (add-hook 'before-save-hook 'elixir-format nil t))))

(use-package js
  :straight nil
  :mode
  ("\\.js\\'" . js-mode)
  :custom
  (js-indent-level 2))

(use-package markdown-mode
  :mode
  ("\\.md\\'" . markdown-mode))

(use-package vue-mode
  :mode ("\\.vue\\'" . vue-mode))

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
  :if (check-hostname "josegpt-desktop")
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
     ?\s-q))
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
