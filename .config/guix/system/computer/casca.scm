(define-module (computer casca)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu services base)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (root-system))

(operating-system
 (inherit %root-operating-system)
 (host-name "casca")

 (bootloader
  (bootloader-configuration
   (bootloader grub-bootloader)
   (target "/dev/sda")))

 (swap-devices
  (list (file-system-label "swap")))
 
 (packages %root-packages)

 (services (cons*
            (udev-rules-service 'backlight %backlight-udev-rule)
            %root-services))

 (file-systems (cdr (%root-file-systems))))
