(define %user-services
  (list
   (make <service>
     #:provides '(gpg-agent)
     #:docstring "Run `gpg-agent`"
     #:respawn? #t
     #:start (make-system-constructor "gpg-connect-agent /bye")
     #:stop (make-system-destructor "gpgconf --kill gpg-agent"))))

(apply register-services %user-services)

;; Send shepherd into the background
(action 'shepherd 'daemonize)

(for-each start %user-services)
