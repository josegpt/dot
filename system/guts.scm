(define-module (system guts)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (system root-system))

(operating-system
 (inherit %root-operating-system)
 (host-name "guts")

 (packages %root-packages)
 (services %root-services)
 (file-systems %root-file-systems))
