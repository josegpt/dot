#!/bin/sh

picom &

feh --bg-scale $HOME/.wallpapers/current.jpg &

exec bspwm
