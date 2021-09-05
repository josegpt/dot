#!/bin/sh

# sxhkd
pgrep -x sxhkd > /dev/null || sxhkd &

# set brightness
xbacklight -set 20%

# xmodmap
xmodmap ~/.xmodmap

# emacs daemon
emacs --daemon

exec herbstluftwm
