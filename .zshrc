ZSH=$HOME/.oh-my-zsh
ZSH_THEME="bira-node"

plugins=(git ruby rvm node extract)

export GIT_EDITOR="emacs -nw"
export EDITOR="emacs -nw"
export VISUAL="emacs -nw"

source $HOME/.zlogin
source $ZSH/oh-my-zsh.sh

export PATH="/home/afronski/.rvm/gems/ruby-1.9.3-p484/bin:/home/afronski/.rvm/gems/ruby-1.9.3-p484@global/bin:/home/afronski/.rvm/rubies/ruby-1.9.3-p484/bin:/home/afronski/.nvm/v0.10.33/bin:/home/afronski/.cabal/bin:/home/afronski/.xmonad/bin:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/bin/vendor_perl:/usr/bin/core_perl:/home/afronski/.cabal/bin:/home/afronski/.xmonad/bin:/home/afronski/.rvm/bin:/home/afronski/.user-scripts:/opt/gradle/gradle-2.0/bin:/usr/local/heroku/bin:/home/afronski/bin/"

export GRADLE_HOME="/opt/gradle/gradle-2.0"

alias grep="/usr/bin/grep $GREP_OPTIONS"
unset GREP_OPTIONS