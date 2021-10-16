(define-module (home josegpt emacs)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (flat packages emacs)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu home-services emacs)
  #:export (config-service))

(define packages
  (map specification->package+output
	     '("emacs-exwm"
         "emacs-corfu"
         "emacs-magit"
         "emacs-envrc"
         "emacs-eglot"
         "emacs-nov-el"
         "emacs-elfeed"
         "emacs-daemons"
         "emacs-go-mode"
         "emacs-elm-mode"
         "emacs-pinentry"
         "emacs-prettier"
         "emacs-pdf-tools"
         "emacs-yaml-mode"
         "emacs-which-key"
         "emacs-yasnippet"
         "emacs-move-text"
         "emacs-marginalia"
         "emacs-ledger-mode"
         "emacs-use-package"
         "emacs-haskell-mode"
         "emacs-geiser-guile"
         "emacs-markdown-mode"
         "emacs-password-store"
         "emacs-typescript-mode"
         "emacs-dockerfile-mode")))

(define config-service
  (service home-emacs-service-type
           (home-emacs-configuration
            (package emacs-native-comp)
            (init-el `(,(slurp-file-gexp (local-file "files/init.el"))))
            (early-init-el `(,(slurp-file-gexp (local-file "files/early-init.el"))))
            (elisp-packages packages)
            (server-mode? #t)
            (xdg-flavor? #t))))
