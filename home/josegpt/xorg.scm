(define-module (home josegpt xorg)
  #:use-module (gnu services)
  #:use-module (home services sx)
  #:use-module (home services picom)
  #:use-module (gnu system keyboard)
  #:use-module (gnu home-services keyboard)
  #:export (picom-config-service
            sx-config-service))

(define picom-config-service
  (service home-picom-service-type
           (home-picom-configuration)))

(define sx-config-service
  (service home-sx-service-type
           (home-sx-configuration
            (exec "emacsclient -c")
            (config '((picom? . #t)
                      ("feh" . "--bg-scale $HOME/.dotfiles/home/josegpt/files/wallpaper.jpg")
                      ("~/.fehbg" . #t)
                      (xhost . "+SI:localuser:$USER"))))))
