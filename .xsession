#!/bin/sh

[ -f $HOME/.profile ] && . $HOME/.profile
[ -f $HOME/.feh ] && $HOME/.feh

xset b off
xsetroot -name "Now shut up and hack"

exec dwm
