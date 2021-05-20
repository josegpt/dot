#!/bin/sh

# x compositor
picom &

# set background
feh --bg-scale ~/.backgrounds/1.jpg

exec emacs
