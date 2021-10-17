(define-module (home josegpt guts)
  #:use-module (gnu home)
  #:use-module ((home josegpt xdg) #:prefix xdg:)
  #:use-module ((home josegpt xorg) #:prefix xorg:)
  #:use-module ((home josegpt mail) #:prefix mail:)
  #:use-module ((home josegpt audio) #:prefix audio:)
  #:use-module ((home josegpt shell) #:prefix shell:)
  #:use-module ((home josegpt emacs) #:prefix emacs:)
  #:use-module ((home josegpt video) #:prefix video:)
  #:use-module ((home josegpt gnupg) #:prefix gnupg:)
  #:use-module ((home josegpt packages) #:prefix packages:)
  #:use-module ((home josegpt version-control) #:prefix vc:))

(home-environment
 (packages (append packages:%desktop packages:%web-browsers))
 (services (list
            vc:git-config-service
            gnupg:config-service
            mail:isync-config-service
            audio:alsa-config-service
            xdg:mime-applications-config-service
            shell:bash-config-service
            emacs:config-service
            emacs:elisp-files-service
            video:mpv-config-service
            xorg:picom-config-service
            xorg:sx-config-service)))
