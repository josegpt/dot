#!/bin/sh

pgrep -x picom > /dev/null || picom &

exec emacsclient -a "" -c
