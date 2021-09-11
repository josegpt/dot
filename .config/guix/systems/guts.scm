(define-module (guts)
  #:use-module (gnu)
  #:use-module (base-system)
  #:use-module (udev-rules)
  #:use-module (gnu services docker)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu packages xorg))

(use-service-modules desktop networking xorg)

(operating-system
 (inherit base-operating-system)
 (host-name "guts")

 (services (cons (service docker-service-type)
                 %desktop-services))

 (file-systems (cons*
                (file-system
                 (mount-point "/boot/efi")
                 (device (uuid "45CD-1679" 'fat32))
                 (type "vfat"))
                (file-system
                 (mount-point "/")
                 (device (file-system-label "system-root"))
                 (type "ext4"))
                %base-file-systems)))
