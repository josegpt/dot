(define-module (root-system)
  #:use-module (gnu system)
  #:use-module (gnu system shadow)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system file-systems)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu services ssh)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services xorg)
  #:use-module (gnu services sound)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu services security-token)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages package-management)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:export (%root-operating-system
            %root-packages
            %root-services
            %root-file-systems
            %backlight-udev-rule))

(define %root-operating-system
  (operating-system
   (host-name "root")
   (timezone "America/New_York")
   (locale "en_US.utf8")
   (keyboard-layout (keyboard-layout "us"))

   (kernel linux)
   (firmware (list linux-firmware))
   (initrd microcode-initrd)

   (bootloader
    (bootloader-configuration
     (bootloader grub-efi-bootloader)
     (targets '("/boot/efi"))
     (keyboard-layout keyboard-layout)))

   (groups (cons*
            (user-group (name "plugdev") (system? #t))
            %base-groups))

   (users
    (cons* (user-account
            (name "josegpt")
            (comment "Jose G Perez Taveras")
            (group "users")
            (home-directory "/home/josegpt")
            (supplementary-groups '("wheel"
                                    "netdev"
                                    "audio"
                                    "plugdev"
                                    "video")))
           %base-user-accounts))

   ;; Operating System needs file-systems to be defined
   (file-systems '())))

(define %root-packages
  (cons*
   git
   stow
   nss-certs
   %base-packages))

(define %root-services
  (cons* (service xorg-server-service-type)
         (service connman-service-type
                  (connman-configuration
                   (disable-vpn? #t)))
         fontconfig-file-system-service
         (service upower-service-type)
         (service wpa-supplicant-service-type)
         (service usb-modeswitch-service-type)
         (accountsservice-service)
         (service colord-service-type)
         (geoclue-service)
         (elogind-service)
         (service openssh-service-type)
         (service pcscd-service-type)
         (dbus-service)
         (service ntp-service-type)
         x11-socket-directory-service
         (service alsa-service-type)
         %base-services))

(define %root-file-systems
  (cons* (file-system
          (mount-point "/boot/efi")
          (device (file-system-label "EFI"))
          (type "vfat"))
         (file-system
          (mount-point "/")
          (device (file-system-label "system-root"))
          (type "ext4"))
         (file-system
          (mount-point "/home")
          (device (file-system-label "home-root"))
          (type "ext4"))
         %base-file-systems))

(define %backlight-udev-rule (udev-rule
                              "90-backlight.rules"
                              (string-append "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                                             "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
                                             "\n"
                                             "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                                             "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))
