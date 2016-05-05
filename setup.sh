echo "Set link to .zshrc"
ln -s $PWD/.zshrc $HOME/.zshrc

echo "Download Oh My ZSH"
git clone --depth=1 https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh

echo "Change default shell to ZSH"
chsh -s /bin/zsh

echo "Setup .emacs.d"
mkdir -p $HOME/.emacs.d
ln -s $PWD/init.el $HOME/.emacs.d/init.el

echo "Set emacs as the default editor"
# For git commits, merges, etc.
export VISUAL=emacs
export EDITOR="$VISUAL"

echo "Setup custom dictionary"
ln -s $PWD/.aspell.en.pws $HOME/.aspell.en.pws

echo "You'll need to logout and login again for the zsh changes to take effect"
