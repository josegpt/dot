(define-module (home josegpt xdg)
  #:use-module (gnu services)
  #:use-module (gnu home services xdg)
  #:export (mime-applications-config-service))

(define mime-applications-config-service
  (service home-xdg-mime-applications-service-type
           (home-xdg-mime-applications-configuration
            (default
              '((x-scheme-handler/http . chromium.desktop)
                (x-scheme-handler/https . chromium.desktop))))))
