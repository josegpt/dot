(define-module (base-system)
  #:use-module (gnu)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages package-management)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

(use-service-modules desktop xorg)

(define-public base-operating-system
  (operating-system
    (host-name "casca")
    (timezone "America/New_York")
    (locale "en_US.utf8")
    
    (kernel linux)
    (firmware (list linux-firmware))
    (initrd microcode-initrd)

    (keyboard-layout (keyboard-layout "us"))

    (services %desktop-services)

    (bootloader 
      (bootloader-configuration
        (bootloader grub-efi-bootloader)
        (target "/boot/efi")
        (keyboard-layout keyboard-layout)))

    (users (cons (user-account
                   (name "josegpt")
                   (comment "Jose G Perez Taveras")
                   (group "users")
                   (home-directory "/home/josegpt")
                   (supplementary-groups '(
                                          "wheel"
                                          "netdev"
                                          "audio"
                                          "video")))
                   %base-user-accounts))

    ;; Install bare-minimum system packages
    (packages (append (list
                        git                    
                        stow
                        emacs
                        nss-certs)
                    %base-packages))

    ;; Guix doesn't like it when there isn't a file-systems
    ;; entry, so add one that is meant to be overridden
    (file-systems (cons*
                   (file-system
                     (mount-point "/tmp")
                     (device "none")
                     (type "tmpfs")
                     (check? #f))
                   %base-file-systems))))