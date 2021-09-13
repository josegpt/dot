(define-module (computer casca)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
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
 (services %root-services)
 (file-systems (cdr (%root-file-systems))))
