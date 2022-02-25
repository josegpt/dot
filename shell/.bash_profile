# ~/.bash_profile

# Defaults
export EDITOR='emacsclient -a "" -r'
export VISUAL="$EDITOR"
export BROWSER="firefox"
export PATH=$HOME/.local/bin:$HOME/go/bin:$PATH
export SSH_AUTH_SOCK='$HOME/.ssh/ssh-agent-env'

# Get ssh-agent pid
[ -f $HOME/.ssh/ssh-agent-env ] && . $HOME/.ssh/ssh-agent-env

# Get the aliases and functions
[ -f $HOME/.bashrc ] && . $HOME/.bashrc
