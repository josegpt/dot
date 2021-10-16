(define-module (home services picom)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (guix packages)
  #:use-module (gnu home services)
  #:use-module (gnu packages compton)
  #:use-module (gnu services configuration)
  #:export (home-picom-configuration
            home-picom-service-type))

(define (serialize-field field-name val)
  (format #f "~%~a={~a;};" field-name val))

(define serialize-string serialize-field)

(define (serialize-alist field-name val)
  (generic-serialize-alist string-append serialize-field val))

(define-configuration home-picom-configuration
  (package
   (package picom)
   "The picom package to use.")
  (config
   (alist '())
   "Association list of key and value pairs for the @file{$XDG_CONFIG_HOME/picom/picom.conf} file.
The value can be a string, G-expression, boolean, number or list
strings."))

(define (picom-files-service config)
  `(("config/picom/picom.conf"
     ,(mixed-text-file
       "picom.conf"
       (serialize-configuration
        config home-picom-configuration-fields)))))

(define (picom-profile-service config)
  (list (home-picom-configuration-package config)))

(define home-picom-service-type
  (service-type
   (name 'home-picom)
   (extensions
    (list (service-extension
           home-files-service-type
           picom-files-service)
          (service-extension
           home-profile-service-type
           picom-profile-service)))
   (description "Install and configure picom")))
