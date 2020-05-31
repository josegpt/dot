# boostrap fisher
if not functions -q fisher
  set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
  curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
  fish -c fisher
end

# set path for cabal bin
set --universal fish_user_paths $fish_user_paths "$HOME"/.cabal/bin

# vi mode
fish_vi_key_bindings

# alias for dotfiles
alias dot="/usr/bin/git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
