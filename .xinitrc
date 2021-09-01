#!/bin/sh

# sxhkd
pgrep -x sxhkd > /dev/null || sxhkd &

# set brightness
brightnessctl s 20%

exec emacs
