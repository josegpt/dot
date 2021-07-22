#!/bin/sh

# sxhkd
pgrep -x sxhkd > /dev/null || sxhkd &

# polybar
pgrep -x polybar > /dev/null || polybar panel &

exec emacs
