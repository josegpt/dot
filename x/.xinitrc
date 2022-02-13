#!/bin/sh

picom &
feh --randomize --bg-scale ~/.config/wallpapers/* &

exec emacs
