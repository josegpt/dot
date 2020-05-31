if not set -q TMUX
  set -g TMUX tmux -f "$HOME"/.config/tmux/session.conf attach
  eval $TMUX
end
