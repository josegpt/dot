#!/bin/sh

# sxhkd
pgrep -x sxhkd > /dev/null || sxhkd &

# lemonbar
pgrep -x lemonbar > /dev/null || ~/.config/lemonbar/bar | lemonbar -b -p &

exec emacs
