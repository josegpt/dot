(define-module (base-system)
  #:use-module (gnu)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages package-management)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

(define-public base-operating-system
  (operating-system
   (host-name "base")
   (timezone "America/New_York")
   (locale "en_US.utf8")

   (kernel linux)
   (firmware (list linux-firmware))
   (initrd microcode-initrd)

   (bootloader
    (bootloader-configuration
     (bootloader grub-efi-bootloader)
     (target "/boot/efi")))

   (users (cons (user-account
                 (name "josegpt")
                 (comment "Jose G Perez Taveras")
                 (group "users")
                 (home-directory "/home/josegpt")
                 (supplementary-groups '("wheel"
                                         "netdev"
                                         "docker"
                                         "audio"
                                         "video")))
                %base-user-accounts))

   ;; Install bare-minimum system packages
   (packages (append (list
                      git
                      stow
                      nss-certs)
                     %base-packages))

   ;; Dummy filesystem
   (file-systems (cons*
                  (file-system
                   (mount-point "/tmp")
                   (device "none")
                   (type "tmpfs")
                   (check? #f))
                  %base-file-systems))))
