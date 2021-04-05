(define-module (nongnu system install)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages package-management)
  #:use-module (nongnu packages linux)
  #:export (installation-os-nonfree))

(define installation-os-nonfree
  (operating-system
   (inherit installation-os)
   (kernel linux)
   (firmware (list linux-firmware))

   ;; Add some extra packages useful for the installation process
   (packages
    (append (list git emacs stow)
            (operating-system-packages installation-os)))))

installation-os-nonfree
