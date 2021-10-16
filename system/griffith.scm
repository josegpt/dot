(define-module (system griffith)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu services base)
  #:use-module (system root-system))

(operating-system
 (inherit %root-operating-system)
 (host-name "griffith")

 (swap-devices
  (list (file-system-label "swap")))

 (packages %root-packages)

 (services (cons*
            (udev-rules-service 'backlight %backlight-udev-rule)
            %root-services))

 (file-systems %root-file-systems))
