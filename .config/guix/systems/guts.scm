(define-module (guts)
  #:use-module (base-system)
  #:use-module (gnu))

(operating-system
  (inherit base-operating-system)
  (host-name "guts")
  
  (file-system (cons*
                  (file-system
                    (mount-point "/boot/efi")
                    (device (uuid "E1D9-0EE2" 'fat32))
                    (type "vfat"))
                  (file-system
                    (mount-point "/")
                    (device (uuid "eaba53d9-d7e5-4129-82c8-df28bfeaa772" 'ext4))
                    (type "ext4"))
                  %base-file-system)))