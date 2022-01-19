#!/bin/sh

export EDITOR='emacsclient -a "" -c'
export VISUAL='$EDITOR'
export BROWSER='firefox'

# Make gnupg work with Yubikey
export GPG_TTY="$(tty)"
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
export PASSWORD_STORE_GPG_OPTS="--no-throw-keyids"
gpg-connect-agent /bye

exec bspwm
