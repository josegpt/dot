export GUILE_LOAD_PATH := $(PWD):$(GUILE_LOAD_PATH)

guts-home:
	guix home reconfigure ./home/josegpt/guts.scm

guts-system:
	sudo -E guix system reconfigure ./system/guts.scm

griffith-home:
	guix home reconfigure ./home/josegpt/griffith.scm

griffith-system:
	sudo -E guix system reconfigure ./system/griffith.scm

casca-home:
	guix home reconfigure ./home/josegpt/casca.scm

casca-system:
	sudo -E guix system reconfigure ./system/casca.scm

.PHONY: guts-home guts-system griffith-home griffith-system casca-home casca-system
