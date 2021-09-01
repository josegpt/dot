#!/bin/sh

# sxhkd
pgrep -x sxhkd > /dev/null || sxhkd &

# set brightness
brightnessctl s 10%

exec emacs
