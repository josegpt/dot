# source confi file for session
source-file "$HOME"/.config/tmux/tmux.conf

WORKSPACE="$HOME"/workspace

# start a new session and open nvim in it
new-session -s josegpt -n dev -d -c $WORKSPACE
send-keys "nvim" Enter
