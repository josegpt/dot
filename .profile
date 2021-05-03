#!/bin/sh

# GET USED TO IT!
setxkbmap -option altwin:ctrl_alt_win

export PATH=$PATH:"~/.yarn/bin"
export HISTFILE=$HOME/.cache/.bash_history

# Default programs
export EDITOR='emacs'
export VISUAL='emacs'
export BROWSER='firefox'

startx
