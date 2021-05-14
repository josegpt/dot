#!/bin/sh

# x compositor
picom &

# GET USED TO IT!
setxkbmap -option ctrl:nocaps

# set background
feh --bg-scale ~/.backgrounds/1.jpg

exec emacs
