![logo](http://asimpson.github.io/dotfiles/logo.svg)

# Install

`git clone git@github.com:asimpson/dotfiles.git ~/.dotfiles`

## Shell Conf
Source `~/.dotfiles/bash/functions` and `~/.dotfiles/bash/aliases` in one of these: `.bash_profile`, `.bashrc`, or `.zshrc`.

## Zsh Conf
Symlink `~/.dotfiles/zsh/.zshrc` to `$HOME`:

  `ln -s ~/.dotfiles/zsh/.zshrc ~/.zshrc`

Note I set a env variable `NAS_IP` pointing to my home NAS in `.zprofile`.

## OS X Conf
* Run `~/.dotfiles/osx/set-defaults.sh` to set system-wide preferences after clean install or on a new machine.
* Run `brew bundle` to install everything in `Brewfile`.

### Launchd
Launchd is OS X's replacement for `cron`.

* Symlink each `.plist` file in `~/.dotfiles/launch-scripts/` into `~/Library/LaunchAgents`

  `ln -s ~/.dotfiles/launch-scripts/com.some-file.plist ~/Library/LaunchAgents/com.some-file.plist`

* Load each daemon by running `launchctl load` for each one:

  `launchctl load ~/Library/LaunchAgents/com.some-name.plist`

## Emacs Conf
* Symlink all files in `~/.dotfiles/emacs` to `~/.emacs.d/`

  `ln -s ~/.dotfiles/emacs/.emacs ~/.emacs.d/`
  `ln -s ~/.dotfiles/emacs/settings.el ~/.emacs.d/`
  `ln -s ~/.dotfiles/emacs/keybinds.el ~/.emacs.d/`

## Vim Conf
* Symlink `~/.dotfiles/.vimrc` to `$HOME`

  `ln -s ~/.dotfiles/.vimrc ~/.vimrc`

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

## GPG Conf

- Install `pinentry-mac` and then tell GPG about it by adding it to `~/.gnupg/gpg-agent.conf` like this:
`pinentry-program /usr/local/bin/pinentry-mac`

- update `~/.gitconfig` with these two values to enable GPG signing:

```
[user]
	signingkey = KEYID
[commit]
  gpgsign = true
```

- `gpgconf --kill gpg-agent` kills the currently running `gpg-agent`. Use this to test config changes.
