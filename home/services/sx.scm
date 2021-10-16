(define-module (home services sx)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (guix packages)
  #:use-module (gnu home services)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu services configuration)
  #:export (home-sx-configuration
            home-sx-service-type))

(define (serialize-field field-name val)
  (cond
   ((boolean? val) (serialize-boolean field-name val))
   (else (format #f "~a ~a~%" field-name val))))

(define (serialize-boolean field-name boolean)
  (let* ((stringified (maybe-object->string field-name))
         (field-name (if (string-suffix? "?" stringified)
                         (string-drop-right stringified 1)
                         field-name)))
    (if boolean
        (serialize-field field-name "&")
        (serialize-field "" ""))))

(define serialize-string serialize-field)

(define (serialize-alist field-name val)
  (generic-serialize-alist string-append serialize-field val))

(define* (executable-plain-file name text)
  "Return an object representing store file NAME containing TEXT.  TEXT is a
a string.

This is equivalent to plain-file, but the file is executable."
  (define build
    (gexp (call-with-output-file (ungexp output "out")
            (lambda (port)
              (display (ungexp text) port)
              (chmod (ungexp output "out") #o755)))))

  (computed-file name build))

(define* (executable-mixed-text-file name #:rest text)
  "Return an object representing store file NAME containing TEXT.  TEXT is a
sequence of strings and file-like objects, as in:

  (mixed-text-file \"profile\"
                   \"export PATH=\" coreutils \"/bin:\" grep \"/bin\")

This is the declarative counterpart of 'text-file*'. The resulting file is made executable."
  (define build
    (gexp (call-with-output-file (ungexp output "out")
            (lambda (port)
              (display (string-append (ungexp-splicing text)) port)
              (chmod (ungexp output "out") #o755)))))

  (computed-file name build))

(define-configuration home-sx-configuration
  (package
   (package sx)
   "The sx package to use.")
  (config
   (alist '())
   "Association list of key and value pairs for the @file{$XDG_CONFIG_HOME/sx/sxrc} file.
The value can be a string, G-expression, boolean, number or list
strings.")
  (exec
   (string "emacs")
   "A string of the command to be call with exec in configuration."))

(define (sx-files-service config)
  `(("config/sx/sxrc"
     ,(executable-mixed-text-file
       "sxrc.sh"
       "#!/bin/sh\n\n"
       (serialize-configuration
        config home-sx-configuration-fields)))))

(define (sx-profile-service config)
  (list (home-sx-configuration-package config)))

(define home-sx-service-type
  (service-type
   (name 'home-sx)
   (extensions
    (list (service-extension
           home-files-service-type
           sx-files-service)
          (service-extension
           home-profile-service-type
           sx-profile-service)))
   (description "Install and configure sx")))
