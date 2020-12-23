(use-package exwm
  :if (string-equal system-type "gnu/linux")
  :bind (:map exwm-mode-map
	      ([?\C-q] . 'exwm-input-send-next-key))
  :config
  (exwm-enable)
  :custom
  (exwm-workspace-number 4)
  (exwm-input-prefix-keys
    '(?\C-x
      ?\C-u
      ?\C-h
      ?\C-g
      ?\M-x
      ?\M-:))
  (exwm-input-global-keys
    `(([?\s-r] . exwm-reset)
      ([?\s-w] . exwm-workspace-switch)
      ([?\s-j] . webjump)
      ([?\s-&] . (lambda (command)
		   (interactive (list (read-shell-command "$ ")))
		   (start-process-shell-command command nil command)))
      ,@(mapcar (lambda (i)
		  `(,(kbd (format "s-%d" i)) .
		     (lambda ()
		       (interactive)
		       (exwm-workspace-switch-create ,i))))
		(number-sequence 0 9))))
  (exwm-input-simulation-keys
    '(([?\C-b] . [left])
      ([?\C-f] . [right])
      ([?\C-p] . [up])
      ([?\C-n] . [down])
      ([?\C-a] . [home])
      ([?\C-e] . [end])
      ([?\C-v] . [next])
      ([?\M-h] . [?\C-a])
      ([?\M-v] . [prior])
      ([?\M-b] . [C-left])
      ([?\M-f] . [C-right])
      ([?\M-<] . [home])
      ([?\M->] . [end])
      ([?\C-c ?g] . [escape])
      ([?\C-c ?k] . [?\C-w])
      ([?\C-\M-b] . [M-left])
      ([?\C-\M-f] . [M-right])
      ([?\C-w] . [?\C-x])
      ([?\M-w] . [?\C-c])
      ([?\C-y] . [?\C-v])
      ([?\C-s] . [?\C-f])
      ([?\C-d] . [delete])
      ([?\C-k] . [S-end delete])
      ([?\M-d] . [C-S-right delete]))))
      