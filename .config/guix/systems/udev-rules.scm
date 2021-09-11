(define-module (udev-rules)
  #:use-module (gnu services base)
  #:export (%backlight-udev-rule))

;;; Allow users of the video group to change screen brightness
(define %backlight-udev-rule (udev-rule
                              "90-backlight.rules"
                              (string-append "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                                             "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
                                             "\n"
                                             "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                                             "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))
