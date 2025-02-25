![logo](http://asimpson.github.io/dotfiles/logo.svg)

## Shell Conf
Source `~/.dotfiles/bash/functions` and `~/.dotfiles/bash/aliases` in one of these: `.bash_profile`, `.bashrc`, or `.zshrc`.

### Zsh Conf
Symlink `~/.dotfiles/zsh/.zshrc` to `$HOME`:

  `ln -s ~/.dotfiles/zsh/.zshrc ~/.zshrc`

### launchd
Launchd is OS X's replacement for `cron`.

* Symlink each `.plist` file in `~/.dotfiles/launch-scripts/` into `~/Library/LaunchAgents`

  `ln -s ~/.dotfiles/launch-scripts/com.some-file.plist ~/Library/LaunchAgents/com.some-file.plist`

* Load each daemon by running `launchctl load` for each one:

  `launchctl load ~/Library/LaunchAgents/com.some-name.plist`

## Emacs Conf
* Symlink all files in `~/.dotfiles/emacs` to `~/.emacs.d/`

  `ln -s ~/.dotfiles/emacs/.emacs ~/.emacs.d/`

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

* Optionally add a SSH key as a signing key:

```
[user]
    signingkey = ssh-rsa SOMEKEY
```
