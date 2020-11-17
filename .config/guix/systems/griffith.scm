(define-module (griffith)
  #:use-module (base-system)
  #:use-module (gnu))

(operating-system
  (inherit base-operating-system)
  (host-name "griffith")
  
  (file-system (cons*
                  (file-system
                    (mount-point "/boot/efi")
                    (device (uuid "1234-ABCD" 'fat32))
                    (type "vfat"))
                  (file-system
                    (mount-point "/")
                    (device (file-system-label "system-root"))
                    (type "ext4"))
                  %base-file-system)))