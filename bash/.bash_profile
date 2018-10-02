source /usr/local/share/chruby/chruby.sh
source /usr/local/share/chruby/auto.sh

RUBIES+=(/usr/local/rubies/*)

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

############################
# gpg-agent
############################
export GPG_TTY=$(tty)
