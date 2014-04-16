# Dotfiles

## Shell Conf
* Source `~/.dotfiles/bash/functions` and `~/.dotfiles/bash/aliases` in one of these: `.bash_profile`, `.bashrc`, or `.zshrc`.

## Zsh Conf
* Symlink `~/.dotfiles/zsh/.zshrc` to `$HOME`

  `ln -s ~/.dotfiles/zsh/.zshrc ~/.zshrc`

* Symlink plugins `~/.dotfiles/zsh/plugins` to `~/.oh-my-zsh/custom`

  `ln -s ~/.dotfiles/zsh/plugins ~/.oh-my-zsh/custom`

* [Download the adamsimpson theme from the releases section of the Github repo](https://github.com/asimpson/dotfiles/releases).

## OS X Conf
* Run `~/.dotfiles/osx/set-defaults.sh` to set system-wide preferences after clean install or on a new machine.

### Launchd
* Launchd is OS X's replacement for `cron`.

* Symlink each `.plist` file in `~/.dotfiles/launch-scripts/` into `~/Library/LaunchAgents`

  `ln -s ~/.dotfiles/launch-scripts/com.some-file.plist ~/Library/LaunchAgents/com.some-file.plist`

* Load each daemon by running `launchctl load` for each one:

  `launchctl load ~/Library/LaunchAgents/com.some-name.plist`

## Tmux Conf
* Symlink `~/.dotfiles/tmux/tmux.conf` to `$HOME`

  `ln -s ~/.dotfiles/tmux/tmux.conf ~/.tmux.conf`

## Vim Conf
* [Install vundle](https://github.com/gmarik/vundle)

* Symlink `~/.dotfiles/.vimrc` to `$HOME`

  `ln -s ~/.dotfiles/.vimrc ~/.vimrc`

* Fire up `vim .vimrc`, and run `:BundleInstall`

* Re-source `.vimrc`, and all vundle bundles should be installed

## Git Conf
* Symlink both `gitmessage.txt` and `master_git_ignore` to `$HOME`

  `ln -s ~/.dotfiles/git/gitmessage.txt ~/.gitmessage.txt`

  `ln -s ~/.dotfiles/git/master_git_ignore ~/.master_git_ignore`

* Add these files to `~/.gitconfig`:

```
    [core]
      excludesfile = /Users/asimpson/.master_git_ignore
    [commit]
      template = /Users/asimpson/.gitmessage.txt
```

* Also add any local modifications to `.gitconfig` like so:

```
    [include]
        path = path/to/local/config/gitconfig.local
```

## Other Dependencies

* [Homebrew](http://brew.sh)
  * node
  * [php](https://github.com/josegonzalez/homebrew-php)
  * mysql
  * git
  * tag
  * tmux
  * reattach-to-user-namespace
