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

;;;; ===> Utils <===
(defvar is-linux-p 
  (string-equal system-type "gnu/linux") "detect if linux is running")

(defun if-pc (name fn &optional args) "call function per system base"
  (when (string-equal system-name name)
    (apply fn args)))

;;;; ===> Settings <===
;;; configure font size
(set-face-attribute 'default nil :height 110)

;;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-Iosvkem t))

;;; Display time
(display-time-mode)

;;; Web Jump
(setq webjump-sites '(("Google" . [simple-query "www.google.com" "www.google.com/search?q=" ""])
		  ("Youtube" . [simple-query "www.youtube.com" "www.youtube.com/results?search_query=" ""])
		  ("AnimeFLV" . [simple-query "www.animeflv.net" "www.animeflv.net/browse?q=" ""])
		  ("Melpa" . [simple-query "melpa.org" "melpa.org/#/?q=" ""])))

;;; matching paren
(show-paren-mode 1)

;;; no blinking
(blink-cursor-mode 0)

;;; disable menu bar
(menu-bar-mode -1)

;;; disable tool bar
(tool-bar-mode -1)

;;; more space
(set-fringe-mode 8)

;;; disbale tooltip
(tooltip-mode -1)

;;; disable scroll bars
(scroll-bar-mode -1)
(toggle-scroll-bar -1)

;;; auto refresh changed file
(global-auto-revert-mode t)

;;; disable statup messages
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

;;; activate line number
(column-number-mode)
(global-display-line-numbers-mode t)

;;; initial load elisp-mode
(setq initial-major-mode 'emacs-lisp-mode)

;;; Don't clutter up directories with files~
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

;;; Don't clutter with #files either
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; Enable Emacs Daemon
(server-mode)

;;;; ===> Package Config <===
;;; Company
(use-package company
  :diminish
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;;; Expand Region
(use-package expand-region
  :diminish
  :bind (("C-=" . er/expand-region)))

;;; Ivy
(use-package ivy
  :diminish
  :init (ivy-mode t)
  :bind
  (("C-s" . swiper))
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t))

;;; Ivy Rich
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

;;; Counsel
(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("C-x b" . counsel-ibuffer)
   ("C-x C-f" . counsel-find-file)
   :map minibuffer-local-map
   ("C-r" . 'counsel-minibuffer-history))
  :custom
  (ivy-initial-inputs-alist nil))

;;; Diminish
(use-package diminish)

;;; Magit
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
  
;;; Move Text
(use-package move-text
  :bind
  (("M-p" . move-text-up)
   ("M-n" . move-text-down)))

;;; Org
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

;;; Rainbow Delimiters
(use-package rainbow-delimiters
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Smartparens
(use-package smartparens
  :diminish
  :hook (prog-mode . smartparens-mode))

;;; Which Key
(use-package which-key
  :diminish
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.5 "include delay to defer its execution"))   

;;;; ===> Language Config <===
;;; Elisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

;;;; ===> EXWM <===
(use-package exwm
  :if is-linux-p
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(exwm which-key smartparens rainbow-delimiters move-text magit diminish counsel ivy-rich ivy expand-region company doom-themes use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
