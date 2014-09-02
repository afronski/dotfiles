[[ -s $HOME/.nvm/nvm.sh ]] && source "$HOME/.nvm/nvm.sh"
[[ -s $HOME/.rvm/scripts/rvm ]] && source "$HOME/.rvm/scripts/rvm"
[[ -s $HOME/.venvburrito/startup.sh ]] && source $HOME/.venvburrito/startup.sh
export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"
