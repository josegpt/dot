#!/bin/sh

gpg-connect-agent /bye
picom &
~/.config/feh/fehbg

exec emacs
