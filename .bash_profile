# .bash_profile

export PATH=$PATH:"~/.yarn/bin"
export HISTFILE=$HOME/.cache/.bash_history

# Get the aliases and functions
[ -f $HOME/.bashrc ] && . $HOME/.bashrc

# Default programs
export EDITOR='emacs'
export VISUAL='emacs'
export BROWSER='firefox'

startx
