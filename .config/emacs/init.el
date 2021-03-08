;;;; Initialize straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
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

;;;; ===> Utils <===
;;; check if linux is running
(defvar is-linux-p
  (string= system-type "gnu/linux")
  "detect if linux is running")

;;; duplicate line
(defun duplicate-line ()
  "Duplicate current line"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank))

;;; apply settings base of hostname
(defun if-pc (name fn &optional args) "call function per system base"
       (when (string-equal system-name name)
         (apply fn args)))

;;; set wallpaper
(defun set-wallpaper ()
  (interactive)
  (start-process-shell-command
   "feh" nil "feh --bg-scale ~/.backgrounds/1.jpg"))

;;;; ===> Per System Config <===
;; activate battery mode
(if-pc "morty" 'display-battery-mode)

;;;; ===> Emacs Config <===
(use-package emacs
  ;; enable emacs daemon
  :init (server-mode)
  :hook (prog-mode . display-line-numbers-mode)
  :bind
  ("<s-return>" . eshell)
  ("M-!" . eshell-command)
  ("C-," . duplicate-line)
  :config
  ;; auto refresh changed file
  (global-auto-revert-mode t)
  ;; transparency
  (add-to-list 'default-frame-alist '(alpha 85 50))
  (set-frame-parameter (selected-frame) 'alpha '(85 50))
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

;;;; ===> Org Config <===
(with-eval-after-load 'org
;;; enable programming languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t))))

(with-eval-after-load 'org
  (require 'org-tempo)
  ;; templates
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

;;;; ===> Package Config <===
;;; achrome theme
(use-package achrome-theme
  :straight (achrome-theme :type git :host github :repo "josegpt/achrome-theme")
  :config
  (load-theme 'achrome t))

;;; auth source pass
(when is-linux-p
  (use-package auth-source-pass
    :after password-store
    :config
    (auth-source-pass-enable)))

;;; company
(use-package company
  :diminish
  :hook (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;;; desktop env
(when is-linux-p
  (use-package desktop-environment
    :diminish
    :after exwm
    :init (desktop-environment-mode)))

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
                  ("https://www.wired.com/feed/rss" tech)
                  ("https://reddit.com/r/unraid.rss" linux)
                  ("https://reddit.com/r/voidlinux.rss" linux)
                  ("https://reddit.com/r/orgmode.rss" emacs org)
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
(when is-linux-p
  (use-package mu4e
    :straight nil
    :defer 10
    :bind
    ("C-c m" . mu4e)
    :custom
    (mu4e-change-filenames-when-moving t)
    (mu4e-view-show-addresses t)
    (mu4e-update-interval (* 10 60))
    (mu4e-get-mail-command "mbsync -a")
    ;; enable inline images
    (mu4e-view-show-images t)
    (mu4e-sent-messages-behavior 'delete)
    (mu4e-confirm-quit nil)
    (mu4e-attachment-dir "~/Downloads")
    (user-full-name "Jose G Perez Taveras")
    (user-mail-address "josegpt27@gmail.com")
    (mu4e-maildir "~/Mail")
    (mu4e-sent-folder "/Sent Mail")
    (mu4e-drafts-folder "/Drafts")
    (mu4e-trash-folder "/Trash")
    (mu4e-maildir-shortcuts
     '((:maildir "/INBOX" :key ?i)
       (:maildir "/Sent Mail" :key ?s)
       (:maildir "/Starred" :key ?r)
       (:maildir "/Spam" :key ?p)
       (:maildir "/Drafts" :key ?d)
       (:maildir "/Trash" :key ?t)))
    :config
    ;; init mu4e
    (mu4e t)
    ;; use imagemagick, if available
    (when (fboundp 'imagemagick-register-types)
      (imagemagick-register-types))))

;;; multiple Cursors
(use-package multiple-cursors
  :bind
  ("C->" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this)
  ("C-c C-<" . 'mc/mark-all-like-this))

;;; password-store
(when is-linux-p
  (use-package password-store
    :bind
    ("C-c p e" . password-store-edit)
    ("C-c p w" . password-store-copy)
    ("C-c p c" . password-store-clear)
    ("C-c p i" . password-store-insert)
    ("C-c p r" . password-store-rename)
    ("C-c p k" . password-store-remove)
    ("C-c p g" . password-store-generate)
    ("C-c p f" . password-store-copy-field)))

;;; pinentry
(when is-linux-p
  (use-package pinentry
    :init (pinentry-start)
    :custom
    ;; epg pinentry
    (epg-pinentry-mode 'loopback)))

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

;;; selectrum
(use-package selectrum
  :after selectrum-prescient
  :config
  (selectrum-mode 1)
  (selectrum-prescient-mode 1))

;;; selectrum for better filtering
(use-package selectrum-prescient)

;;; time
(use-package time
  :straight nil
  :config
  (display-time-mode t)
  :custom
  (display-time-default-load-average nil)
  (display-time-format "%I:%M%P  %B %d, %Y(%a)"))

;;; web jump
(use-package webjump
  :straight nil
  :bind
  ("C-c j" . webjump)
  :custom
  (webjump-sites '(("URL" . [simple-query "about:blank" "www." ""])
                   ("Reddit" . [simple-query "reddit.com" "reddit.com/search/?q=" ""])
                   ("Google" . [simple-query "google.com" "www.google.com/search?q=" ""])
                   ("Github" . [simple-query "github.com" "github.com/search?q=" ""])
                   ("AnimeFLV" . [simple-query "animeflv.net" "animeflv.net/browse?q=" ""])
                   ("Youtube" . [simple-query "youtube.com" "youtube.com/results?search_query=" ""])
                   ("Crunchyroll" . [simple-query "crunchyroll.com" "crunchyroll.com/search?&q=" ""])
                   ("WhatsApp" . "web.whatsapp.com")
                   ("Telegram" . "web.telegram.org")
                   ("Discord" . "discord.com/app")
                   ("Gmail" . "mail.google.com")
                   ("Melpa" . [simple-query "melpa.org" "melpa.org/#/?q=" ""]))))

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

;;; yasnippet
(use-package yasnippet
  :diminish (yas-minor-mode)
  :hook ((prog-mode text-mode) . yas-minor-mode)
  :config
  (yas-reload-all))

;;;; ===> Language Config <===
;;; eldoc
(use-package eldoc
  :straight nil
  :diminish
  :hook ((emacs-lisp-mode lisp-interaction-mode) . eldoc-mode))

;;; markdown
(use-package markdown-mode
  :mode
  ("\\.md\\'" . markdown-mode))

;;; prettier
(use-package prettier-js
  :hook ((js-mode web-mode) . prettier-js))

;;;; ===> EXWM <===
(when is-linux-p
  (use-package exwm
    :init (exwm-enable)
    :hook
    ;; Make class name the buffer name
    (exwm-update-class . (lambda ()
                           (exwm-workspace-rename-buffer exwm-class-name)))
    (exwm-update-title . (lambda ()
                           (pcase exwm-class-name
                             ("Chromium" (exwm-workspace-rename-buffer (format "Chromium: %s" exwm-title))))))
    ;; send window to workspace
    (exwm-manage-finish . (lambda ()
                            (pcase exwm-class-name
                              ("Chromium" (exwm-workspace-move-window 1)))))
    :bind
    (:map exwm-mode-map
          ("C-q" . exwm-input-send-next-key))
    :config
    (set-wallpaper)
    :custom
    (exwm-workspace-number 4)
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
       ([?\M-d] . [C-S-right delete])))))
