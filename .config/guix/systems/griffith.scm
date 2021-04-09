(define-module (griffith)
  #:use-module (base-system)
  #:use-module (udev-rules)
  #:use-module (gnu)
  #:use-module (gnu services docker)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu packages xorg))

(use-service-modules desktop networking xorg)

(operating-system
 (inherit base-operating-system)
 (host-name "griffith")

 (services (cons* (service docker-service-type)
                  (udev-rules-service 'backlight %backlight-udev-rule)
                  %desktop-services))

 (file-systems (cons*
                (file-system
                 (mount-point "/boot/efi")
                 (device (uuid "2FF3-1305" 'fat32))
                 (type "vfat"))
                (file-system
                 (mount-point "/")
                 (device (file-system-label "system-root"))
                 (type "ext4"))
                %base-file-systems)))
