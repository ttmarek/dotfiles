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

# nvm
[[ -s ~/.nvm/nvm.sh ]] && . ~/.nvm/nvm.sh

# rvm
export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
