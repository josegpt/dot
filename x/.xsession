#!/bin/sh

[ -f $HOME/.profile ] && . $HOME/.profile

xset b off

exec bspwm
