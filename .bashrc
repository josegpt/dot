# aliases
alias ls='ls --color=auto'
alias ll='ls -lh'
alias la='ls -lah'
alias rb='doas reboot'
alias po='doas poweroff'
alias xqi='xbps-query -m'
alias xqo='xbps-query -O'
alias xqs='xbps-query -s'
alias xq='xbps-query -Rs'
alias xi='doas xbps-install -Su'
alias xr='doas xbps-remove -R'
alias xro='doas xbps-remove -o'
alias c='clear'

PS1='\W $ '

eval "$(direnv hook bash)"
