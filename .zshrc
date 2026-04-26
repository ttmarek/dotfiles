DOTFILES="$HOME/Developer/dotfiles"

for config in "$DOTFILES"/zsh/*.zsh; do
  source "$config"
done
