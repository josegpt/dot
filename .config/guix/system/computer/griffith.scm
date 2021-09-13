(define-module (computer griffith)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (root-system))

(operating-system
 (inherit %root-operating-system)
 (host-name "griffith")

 (swap-devices
  (list (file-system-label "swap")))

 (packages %root-packages)
 (services %root-services)
 (file-systems %root-file-systems))
