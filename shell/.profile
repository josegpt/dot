#!/bin/sh

export EDITOR='emacsclient -a "" -c'
export VISUAL="$EDITOR"
export BROWSER="firefox"
export PATH=$HOME/bin:$HOME/go/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/games
export ENV=$HOME/.kshrc
export LC_CTYPE="en_US.UTF-8"
