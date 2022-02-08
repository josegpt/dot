#!/bin/sh

gpg-connect-agent reloadagent /bye
picom &
feh --randomize --bg-scale ~/.config/wallpapers/* &

exec emacs
