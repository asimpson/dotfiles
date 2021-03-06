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

export GPG_TTY=$(tty)

# export PATH="$HOME/.cargo/bin:$PATH"
# export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

export NVM_DIR="$HOME/Source/nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export PATH="$HOME/.cargo/bin:$PATH"
export PATH="/usr/local/go/bin:$PATH"
export TERMINAL="/usr/bin/tilix"
if [ -e /home/asimpson/.nix-profile/etc/profile.d/nix.sh ]; then . /home/asimpson/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
