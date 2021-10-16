(define-module (home josegpt packages)
	#:use-module (gnu packages)
  #:export (%desktop
            %laptop
            %web-browsers))

(define %desktop
  (map specification->package+output
       '("mu"
         "feh"
         "curl"
         "xhost"
         "ledger"
         "direnv"
         "setxkbmap"
         "alsa-utils"
         "font-iosevka"
         "pinentry-emacs")))

(define %laptop
  (map specification->package+output
	     '("light")))

(define %web-browsers
  (map specification->package+output
	     '("ungoogled-chromium"
         "ublock-origin-chromium")))
