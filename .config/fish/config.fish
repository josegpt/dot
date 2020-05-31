# boostrap fisher
if not functions -q fisher
  set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
  curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
  fish -c fisher
end

# set path for stack
set -g PATH "$HOME"/.local/bin $PATH
set -g PATH /usr/local/bin $PATH

# vi mode
fish_vi_key_bindings

# alias for dotfiles
alias dot="/usr/bin/git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
