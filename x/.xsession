#!/bin/sh

[ -f $HOME/.profile ] && . $HOME/.profile

# Defaults
export EDITOR='emacsclient -a "" -r'
export VISUAL='$EDITOR'
export BROWSER='firefox'

# Make gnupg work with Yubikey
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
export PASSWORD_STORE_GPG_OPTS="--no-throw-keyids"
gpg-connect-agent /bye

picom &
~/.config/feh/fehbg

emacs
