#!/bin/sh

[ -f $HOME/.profile ] && . $HOME/.profile

xset b off
~/.config/feh/fehbg &
xcompmgr -c -f -D 2 &

exec emacsclient -a "" -c
