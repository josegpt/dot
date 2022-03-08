#!/bin/sh

[ -f $HOME/.profile ] && . $HOME/.profile

pgrep -x picom > /dev/null || picom &

exec emacs
