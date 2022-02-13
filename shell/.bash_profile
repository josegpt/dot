# ~/.bash_profile

if [ ! -S "$HOME/.ssh/ssh_auth_sock" ]; then
  eval $(ssh-agent)
  ln -sf "$SSH_AUTH_SOCK" $HOME/.ssh/ssh_auth_sock
fi

# Defaults
export EDITOR='emacsclient -a "" -r'
export VISUAL='$EDITOR'
export BROWSER='firefox'
export PATH=$HOME/.local/bin:$HOME/go/bin:$PATH
export SSH_AUTH_SOCK='$HOME/.ssh/ssh_auth_sock'

# Get the aliases and functions
[ -f $HOME/.bashrc ] && . $HOME/.bashrc
