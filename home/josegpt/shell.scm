(define-module (home josegpt shell)
  #:use-module (gnu services)
  #:use-module (gnu home services shells)
  #:export (bash-config-service))

(define bash-config-service
  (service home-bash-service-type
           (home-bash-configuration
            (guix-defaults? #t)
            (environment-variables
             '(("SSH_AUTH_SOCK" . "$(gpgconf --list-dirs agent-ssh-socket)")
               ("HISTFILE" . "$XDG_CACHE_HOME/.bash_history"))))))
