(define-module (griffith)
  #:use-module (base-system)
  #:use-module (gnu))

(operating-system
 (inherit base-operating-system)
 (host-name "griffith")

 (swap-devices (list
                (file-system-label "swap")))

 (file-systems (cons*
                (file-system
                 (mount-point "/boot/efi")
                 (device (file-system-label "EFI"))
                 (type "vfat"))
                (file-system
                 (mount-point "/")
                 (device (file-system-label "system-root"))
                 (type "ext4"))
                %base-file-systems)))
