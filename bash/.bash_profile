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

PATH="/usr/local/sbin:$PATH"
PATH="/usr/local/bin:$PATH"
PATH="/Users/asimpson/.npm-global/bin:$PATH"

############################
# gpg-agent
############################
export GPG_TTY=$(tty)

export PATH="$HOME/.cargo/bin:$PATH"
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
