export GUILE_LOAD_PATH := $(PWD):$(GUILE_LOAD_PATH)

guts-home:
	guix home reconfigure ./home/josegpt/guts.scm

guts-system:
	sudo -E guix system reconfigure ./system/guts.scm

.PHONY: guts-home guts-system
