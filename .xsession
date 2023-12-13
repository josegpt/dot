#!/bin/sh

[ -f $HOME/.profile ] && . $HOME/.profile

xset b off
xsetroot -name "Now shut up and hack"

exec dwm
