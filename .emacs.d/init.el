(package-initialize)

(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
		   user-emacs-directory))

(require 'exwm)
(require 'exwm-config)
(exwm-config-default)
