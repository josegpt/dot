;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; matching paren
(show-paren-mode 1)

;; no blinking
(blink-cursor-mode 0)

;; disable menu bar
(menu-bar-mode -1)

;; disable tool bar
(tool-bar-mode -1)

;; more space
(set-fringe-mode 8)

;; disbale tooltip
(tooltip-mode -1)

;; disable scroll bars
(scroll-bar-mode -1)
(toggle-scroll-bar -1)

(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

;; activate line number
(column-number-mode)
(global-display-line-numbers-mode t)

;; disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
			treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; enable emacs daemon
(server-mode)

;; configure font size
(set-face-attribute 'default nil :height 110)

(use-package doom-themes
  :config
  (load-theme 'doom-Iosvkem t))

(use-package diminish)

(global-auto-revert-mode t)

;; place autosave files somewhere else
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transform '((".*" "~/.config/emacs/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.config/emacs/backups")))
 '(package-selected-packages
   '(magit which-key use-package smartparens rainbow-delimiters move-text ivy-rich ivy-posframe exwm expand-region doom-themes diminish darkokai-theme counsel company)))

;; create autosave folder if necessary
(make-directory "~/.config/emacs/autosaves/" t)

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

;; Automatically tangle our Emacs.org config file when we save it
(defun org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/.dotfiles/EMACS.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'org-babel-tangle-config)))

(use-package which-key
  :diminish
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.5 "include delay to defer its execution"))

(use-package ivy
  :diminish
  :init (ivy-mode t)
  :bind
  (("C-s" . swiper))
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t))

(use-package ivy-posframe
  :diminish
  :init (ivy-posframe-mode t)
  :custom
  (ivy-posframe-width 115)
  (ivy-posframe-min-width 115)
  (ivy-posframe-height 10)
  (ivy-posframe-min-height 10)
  (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-parameters '((parent-frame . nil)
			     (left-fringe . 8)
			     (right-fringe . 8))))

(use-package ivy-rich
  :init (ivy-rich-mode t)
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

(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("C-x b" . counsel-ibuffer)
   ("C-x C-f" . counsel-find-file)
   :map minibuffer-local-map
   ("C-r" . 'counsel-minibuffer-history))
  :custom
  (ivy-initial-inputs-alist nil))

(use-package webjump
  :custom
  webjump-sites '(("Google" . [simple-query "www.google.com" "www.google.com/search?q=" ""])
		  ("Youtube" . [simple-query "www.youtube.com" "www.youtube.com/results?search_query=" ""])
		  ("AnimeFLV" . [simple-query "www.animeflv.net" "www.animeflv.net/browse?q=" ""])
		  ("Melpa" . [simple-query "melpa.org" "melpa.org/#/?q=" ""])))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

(use-package expand-region
  :diminish
  :bind (("C-=" . er/expand-region)))

(use-package company
  :diminish
  :hook (prog-mode . company-mode))

(use-package move-text
  :bind
  (("M-p" . move-text-up)
   ("M-n" . move-text-down)))

(use-package rainbow-delimiters
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package smartparens
  :diminish
  :hook (prog-mode . smartparens-mode))

(use-package exwm
  :bind (:map exwm-mode-map
	      ([?\C-q] . 'exwm-input-send-next-key))
  :config
  (exwm-enable)
  :custom
  (exwm-workspace-number 4)
  (exwm-input-prefix-keys
    '(?\C-x
      ?\C-u
      ?\C-h
      ?\C-g
      ?\M-x
      ?\M-:))
  (exwm-input-global-keys
    `(([?\s-r] . exwm-reset)
      ([?\s-w] . exwm-workspace-switch)
      ([?\s-j] . webjump)
      ([?\s-&] . (lambda (command)
		   (interactive (list (read-shell-command "$ ")))
		   (start-process-shell-command command nil command)))
      ,@(mapcar (lambda (i)
		  `(,(kbd (format "s-%d" i)) .
		     (lambda ()
		       (interactive)
		       (exwm-workspace-switch-create ,i))))
		(number-sequence 0 9))))
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
      ([?\C-c ?g] . [escape])
      ([?\C-c ?k] . [?\C-w])
      ([?\C-\M-b] . [M-left])
      ([?\C-\M-f] . [M-right])
      ([?\C-w] . [?\C-x])
      ([?\M-w] . [?\C-c])
      ([?\C-y] . [?\C-v])
      ([?\C-s] . [?\C-f])
      ([?\C-d] . [delete])
      ([?\C-k] . [S-end delete])
      ([?\M-d] . [C-S-right delete]))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
