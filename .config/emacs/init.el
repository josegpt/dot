;;;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; debug purposes
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
(defvar is-linux-p (string= system-type "gnu/linux") "detect if linux is running")

;;; duplicate line
;;; taken from https://github.com/rexim/dotfiles/blob/master/.emacs.rc/misc-rc.el
(defun duplicate-line ()
  "Duplicate current line"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank))

;;; kill and remove window
(defun k-r-buffer ()
  (interactive)
  (let ((is-essential-buffers-p (or
                                 (string= "*scratch*" (buffer-name))
                                 (string= "*Messages*" (buffer-name))))
        (no-windows (count-windows)))
    (if (not is-essential-buffers-p)
        (if (> no-windows 1)
            (kill-buffer-and-window)
          (kill-current-buffer))
      (message "Trying to kill/remove essential buffer/window."))))

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
  ("s-c" . k-r-buffer)
  ("C-," . duplicate-line)
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; configure font size
  (set-face-attribute 'default nil :family "Iosevka" :height 130)
  ;; activate line number
  (column-number-mode -1)
  ;; theme
  (load-theme 'wombat t)
  ;; disable menu bar
  (menu-bar-mode -1)
  ;; matching paren
  (show-paren-mode t)
  ;; disable tool bar
  (tool-bar-mode -1)
  ;; more space
  (set-fringe-mode 1)
  ;; disbale tooltip
  (tooltip-mode -1)
  ;; disable scroll bars
  (scroll-bar-mode -1)
  (toggle-scroll-bar -1)
  ;; auto refresh changed file
  (global-auto-revert-mode t)
  ;; transparency
  (set-frame-parameter (selected-frame) 'alpha '(85 50))
  (add-to-list 'default-frame-alist '(alpha 85 50))
  ;; no blinking
  (blink-cursor-mode -1)
  :custom
  ;; tabs mode
  (indent-tabs-mode nil)
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
;;; auth source pass
(use-package auth-source-pass
  :after pass
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
  ("C-x b" . counsel-ibuffer)
  ("C-x C-f" . counsel-find-file)
  :custom
  (ivy-initial-inputs-alist nil))

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

;;; ivy
(use-package ivy
  :diminish
  :init (ivy-mode t)
  :bind
  ("C-s" . swiper)
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t))

;;; ivy rich
(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode t)
  :custom
  (ivy-format-function #'ivy-format-function-line)
  (ivy-rich-display-transformer-list
   '(ivy-switch-buffer
     (:columns
      ((ivy-rich-candidate (:width 40))
       (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
       (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
       (ivy-rich-switch-buffer-project (:width 15 :face success))
       (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3)))))
       :predicate
       (lambda (cand)
         (if-let ((buffer (get-buffer cand)))
             (with-current-buffer buffer
               (not (derived-mode-p 'exwm-mode))))))))))

;;; magit
(use-package magit
  :bind
  ("C-x g" . magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;;; move Text
(use-package move-text
  :bind
  (("M-p" . move-text-up)
   ("M-n" . move-text-down)))

;;; mu4e
(when is-linux-p
  (use-package mu4e
    :ensure nil
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

;;; pass
(when is-linux-p
  (use-package pass
    :bind
    ("C-c p" . pass)))

;;; pinentry
(when is-linux-p
  (use-package pinentry
    :init (pinentry-start)
    :custom
    ;; epg pinentry
    (epg-pinentry-mode 'loopback)))

;;; rainbow delimiters
(use-package rainbow-delimiters
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

;;; smartparens
(use-package smartparens
  :diminish
  :hook (prog-mode . smartparens-mode)
  :config
  (sp-use-paredit-bindings))

;; time
(use-package time
  :config
  (display-time-mode t)
  :custom
  (display-time-default-load-average nil)
  (display-time-format "%I:%M%P  %B %d, %Y(%a)"))

;;; web jump
(use-package webjump
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

;; whitespace
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

;; windmove
(use-package windmove
  :bind
  ("s-p" . windmove-up)
  ("s-n" . windmove-down)
  ("s-b" . windmove-left)
  ("s-f" . windmove-right)
  ("s-P" . windmove-swap-states-up)
  ("s-N" . windmove-swap-states-down)
  ("s-B" . windmove-swap-states-left)
  ("s-F" . windmove-swap-states-right)
  ("C-c w p" . windmove-delete-up)
  ("C-c w n" . windmove-delete-down)
  ("C-c w b" . windmove-delete-left)
  ("C-c w f" . windmove-delete-right))

;;;; ===> Language Config <===
;;; eldoc
(use-package eldoc
  :diminish
  :hook ((emacs-lisp-mode lisp-interaction-mode) . eldoc-mode))

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
       ?\M-!
       ?\s-c
       ?\s-p
       ?\s-n
       ?\s-f
       ?\s-b))
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(pinentry elfeed pass exwm which-key rainbow-delimiters multiple-cursors move-text magit diminish counsel ivy-rich ivy expand-region desktop-environment use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
