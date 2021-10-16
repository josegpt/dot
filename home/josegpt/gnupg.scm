(define-module (home josegpt gnupg)
  #:use-module (gnu services)
  #:use-module (gnu home-services gnupg)
  #:export (config-service))

(define config-service
  (service home-gnupg-service-type
           (home-gnupg-configuration
            (gpg-config
             (home-gpg-configuration
              (extra-config
               '((personal-cipher-preferences . ("AES256"
                                                 "AES192"
                                                 "AES"))
                 (personal-digest-preferences . ("SHA512"
                                                 "SHA384"
                                                 "SHA256"))
                 (personal-compress-preferences . ("ZLIB"
                                                   "BZIP2"
                                                   "ZIP"
                                                   "Uncompressed"))
                 (default-preference-list . ("SHA512"
                                             "SHA384"
                                             "SHA256"
                                             "AES256"
                                             "AES192"
                                             "AES"
                                             "ZLIB"
                                             "BZIP2"
                                             "ZIP"
                                             "Uncompressed"))
                 (cert-digest-algo . "SHA512")
                 (s2k-digest-algo . "SHA512")
                 (s2k-cipher-algo . "AES256")
                 (charset . "utf-8")
                 (fixed-list-mode? . #t)
                 (no-comments? . #t)
                 (no-emit-version? . #t)
                 (no-greeting? . #t)
                 (keyid-format . "0xlong")
                 (list-options . "show-uid-validity")
                 (verify-options . "show-uid-validity")
                 (with-fingerprint? . #t)
                 (require-cross-certification? . #t)
                 (no-symkey-cache? . #t)
                 (use-agent? . #t)
                 (throw-keyids? . #t)))))
            (gpg-agent-config
             (home-gpg-agent-configuration
              (ssh-agent? #t)
              (pinentry-flavor 'emacs)
              (extra-config
               '((ttyname . "$GPG_TTY")
                 (default-cache-ttl . 60)
                 (max-cache-ttl . 120))))))))
