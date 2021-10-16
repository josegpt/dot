(define-module (home josegpt video)
  #:use-module (gnu services)
  #:use-module (gnu home-services video)
  #:export (mpv-config-service))

(define mpv-config-service
  (service home-mpv-service-type
           (home-mpv-configuration
            (default-options
              '((hwdec . "auto-safe")
                (ytdl-format . "bestvideo[height<=?1080]+bestaudio/best"))))))
