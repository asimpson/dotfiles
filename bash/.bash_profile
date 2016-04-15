[[ -s "$HOME/.profile" ]] && source "$HOME/.profile" # Load the default .profile
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

if [ -f ~/.dotfiles/bash/functions ]; then
  source ~/.dotfiles/bash/functions
fi

if [ -f ~/.dotfiles/bash/aliases ]; then
  source ~/.dotfiles/bash/aliases
fi

PATH="$PATH:/usr/local/share/npm/bin"
PATH="$PATH:/usr/local/lib/node_modules"
PATH="/usr/local/sbin:$PATH"
PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
PATH="/usr/local/bin:$PATH"

############################
# gpg-agent
############################
GPG_AGENT="/usr/local/bin/gpg-agent"
export GPG_TTY=$(tty)

if [ -f ${GPG_AGENT} ]; then
  . ~/.bash_gpg
fi
