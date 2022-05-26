#!/bin/sh

[ -f $HOME/.profile ] && . $HOME/.profile

xset b off
~/.fehbg &
xcompmgr -c -f -D 2 &

exec emacsclient -a "" -c
