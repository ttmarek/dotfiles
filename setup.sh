echo "Set link to .zshrc"
ln -s $PWD/.zshrc $HOME/.zshrc

echo "Download Oh My ZSH"
git clone --depth=1 https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh

echo "Change default shell to ZSH"
chsh -s /bin/zsh
