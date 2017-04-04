# Oh My ZSH
# ---------
# Example at:
# https://github.com/robbyrussell/oh-my-zsh/blob/master/templates/zshrc.zsh-template

export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="robbyrussell"

# Use case sensitive completion
CASE_SENSITIVE="true"

# Disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Disable marking untracked files under VCS as dirty. This makes
# repository status check for large repositories much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

plugins=(git)

# User configuration
export PATH=$HOME/bin:/usr/local/bin:$PATH

source $ZSH/oh-my-zsh.sh

alias ctrlnocaps="/usr/bin/setxkbmap -option 'ctrl:nocaps'"

# Make a directory and cd into it
mkcdir () {
    mkdir -p -- "$1" &&
      cd -P -- "$1"
}

# Allow emacs to track directory
if [ -n "$INSIDE_EMACS" ]; then
  chpwd() { print -P "\033AnSiTc %d" }
  print -P "\033AnSiTu %n"
  print -P "\033AnSiTc %d"
fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
