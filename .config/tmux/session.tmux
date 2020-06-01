# source confi file for session
source-file "$HOME"/.config/tmux/tmux.conf

WORKSPACE="$HOME"/workspace

# start a new session and open nvim in it
new-session -s josegpt -n dev -d -c $WORKSPACE
send-keys "nvim" Enter

# split window horizontally with 30 % width and
# split vertically
split-window -h -p 30 -c $WORKSPACE
split-window -v -c $WORKSPACE

# select pane with nvim
select-pane -t 0
