(define-module (griffith)
  #:use-module (base-system)
  #:use-module (gnu))

(operating-system
 (inherit base-operating-system)
 (host-name "griffith")

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
