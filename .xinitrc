#!/bin/sh

# x compositor
picom &

# GET USED TO IT!
setxkbmap -option altwin:ctrl_alt_win

# set background
feh --bg-scale ~/.backgrounds/1.jpg

exec emacs
