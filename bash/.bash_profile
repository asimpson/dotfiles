[[ -s "$HOME/.profile" ]] && source "$HOME/.profile" # Load the default .profile

if [ -f ~/.dotfiles/bash/functions ]; then
  source ~/.dotfiles/bash/functions
fi

if [ -f ~/.dotfiles/bash/aliases ]; then
  source ~/.dotfiles/bash/aliases
fi

PATH="$PATH:/usr/local/share/npm/bin"
PATH="$PATH:/usr/local/lib/node_modules"
PATH="/usr/local/sbin:$PATH"
PATH="/usr/local/bin:$PATH"
PATH="/Users/asimpson/.better-npm/bin:$PATH"
PATH="/Users/asimpson/.better-npm/lib/node_modules:$PATH"
PATH="$HOME/.cargo/bin:$PATH"

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

############################
# gpg-agent
############################
export GPG_TTY=$(tty)
