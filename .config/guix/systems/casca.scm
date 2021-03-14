(define-module (guts)
  #:use-module (base-system)
  #:use-module (gnu))

(operating-system
 (inherit base-operating-system)
 (host-name "casca")

 (bootloader
    (bootloader-configuration
     (bootloader grub-bootloader)
     (target "/dev/sda1")
     (keyboard-layout keyboard-layout)))

 (file-systems (cons*
                (file-system
                 (mount-point "/boot/efi")
                 (device (uuid "1234-ABCD" 'fat32))
                 (type "vfat"))
                (file-system
                 (mount-point "/")
                 (device (file-system-label "system-root"))
                 (type "ext4"))
                %base-file-systems)))
