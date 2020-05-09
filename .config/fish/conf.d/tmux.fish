if not set -q TMUX
  set -g TMUX tmux new-session -d -s dev
  eval $TMUX
  tmux attach-session -d -t dev
end
