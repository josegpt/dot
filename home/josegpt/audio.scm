(define-module (home josegpt audio)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:export (alsa-config-service))

(define alsa-config-service
  (simple-service
   'alsa-config
   home-files-service-type
   (list `("asoundrc"
           ,(plain-file "asound.conf"
                        "defaults.ctl.card 1;\ndefaults.pcm.card 1;")))))
