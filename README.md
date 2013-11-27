# Dotfiles install

## Shell aliases and functions
* Add `. ~/.dotfiles/bash/functions
. ~/.dotfiles/bash/aliases` to your .bash_profile, .bashrc, or .zshrc to get aliases and functions working.

## Zsh config
* Symlink `~/.dotfiles/zsh/.zshrc` to `$HOME`

* Symlink plugins `~/.dotfiles/zsh/plugins` to `~/.oh-my-zsh/custom`

* [Download the theme from the releases section of the Github repo](https://github.com/asimpson/dotfiles/releases).

## Launchd daemons
* Symlink the `.plist` files in `.dotfiles/launch-scripts/` into `~/Library/LaunchAgents`

  `ln -s ~/.dotfiles/launch-scripts/com.some-file.plist ~/Library/LaunchAgents/com.some-file.plist`

* Load the daemons by running the `load` command for each plist:

  `launchctl load ~/Library/LaunchAgents/com.some-name.plist`

## Mac OS X Defaults
* Run the `set-defaults.sh` script in `.dotfiles/osx/` to set the preferences.

## Tmux Conf
* Symlink the `.dotfiles/tmux/tmux.conf` file to your `$HOME` directory

## Vim Config
* [Install vundle](https://github.com/gmarik/vundle)

* Symlink `~/.dotfiles/.vimrc` to $HOME

* Open vim and `.vimrc`, and run `:BundleInstall`

* Re-source `.vimrc`, and all bundles should be installed

## Git Config
* Symlink both `gitmessage.txt` and `master_git_ignore` into $HOME

* Add `excludesfile = ~/.master_git_ignore` under `[core]` to `~/.gitconfig` 

* Add `template = ~/.gitmessage.txt` under `[commit]` to `~/.gitconfig` 
