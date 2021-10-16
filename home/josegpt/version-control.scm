(define-module (home josegpt version-control)
  #:use-module (gnu services)
  #:use-module (gnu home-services version-control)
  #:export (git-config-service))


(define git-config-service
  (service
   home-git-service-type
   (home-git-configuration
    (config
     `((user
        ((name . "Jose G Perez Taveras")
         (email . "josegpt27@gmail.com")
         (editor . "emacs")))
       (init
        ((defaultBranch . "main")))
       (github
        ((user . "josegpt"))))))))
