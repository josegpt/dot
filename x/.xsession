#!/bin/sh

[ -f $HOME/.profile ] && . $HOME/.profile

pgrep -x picom > /dev/null || picom &

xset b off

exec emacs
