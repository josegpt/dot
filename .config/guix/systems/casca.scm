(define-module (casca)
  #:use-module (base-system)
  #:use-module (udev-rules)
  #:use-module (gnu)
  #:use-module (gnu services docker)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu packages xorg))

(operating-system
 (inherit base-operating-system)
 (host-name "casca")

 (bootloader
  (bootloader-configuration
   (bootloader grub-bootloader)
   (target "/dev/sda")))

 (swap-devices (list
                (file-system-label "swap")))

 (services (cons* (service docker-service-type)
                  (udev-rules-service 'backlight %backlight-udev-rule)
                  %desktop-services))

 (file-systems (cons*
                (file-system
                 (mount-point "/")
                 (device (file-system-label "system-root"))
                 (type "ext4"))
                %base-file-systems)))
