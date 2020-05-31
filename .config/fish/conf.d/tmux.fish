if not set -q TMUX
  set -g TMUX tmux -f "$HOME"/.config/tmux/session.tmux attach
  eval $TMUX
end
