#!/bin/sh

# sxhkd
pgrep -x sxhkd > /dev/null || sxhkd &

exec emacs
