# If not running interactively, don't do anything
[[ $- != *i* ]] && return

bind 'set completion-ignore-case on'
shopt -s cdspell
shopt -s autocd

# aliases
alias ll='ls -lh'
alias la='ls -lah'
alias ls='ls -lh --color=auto'
alias rb='doas reboot'
alias po='doas poweroff'
alias sd='doas shutdown'
alias xqi='xbps-query -m'
alias xqo='xbps-query -O'
alias xqs='xbps-query -s'
alias xq='xbps-query -Rs'
alias xi='doas xbps-install -Su'
alias xr='doas xbps-remove -R'
alias xro='doas xbps-remove -o'

PS1='\w '

