#!/bin/sh

picom &
$HOME/.fehbg &

exec emacsclient -c
# exec emacs
