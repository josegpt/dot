# ~/.bash_profile

# Defaults
export EDITOR='emacsclient -a "" -c'
export VISUAL='$EDITOR'
export BROWSER='firefox'
export XDG_RUNTIME_DIR='/tmp'

# Make gnupg work with Yubikey
export GPG_TTY="$(tty)"
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
export PASSWORD_STORE_GPG_OPTS="--no-throw-keyids"
gpg-connect-agent /bye

# Get the aliases and functions
[ -f $HOME/.bashrc ] && . $HOME/.bashrc

[ "$(tty)" = "/dev/tty1" ] && exec cagebreak
