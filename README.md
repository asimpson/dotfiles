![logo](http://asimpson.github.io/dotfiles/logo.svg)

### Zsh Conf
Symlink `~/.dotfiles/zsh/.zshrc` to `$HOME`:

  `ln -s ~/.dotfiles/zsh/.zshrc ~/.zshrc`

## Emacs Conf
* Symlink all files in `~/.dotfiles/emacs` to `~/.emacs.d/`

  `ln -s ~/.dotfiles/emacs/.emacs ~/.emacs.d/`

### non NixOS install

```sh
nix profile install --impure --expr '
let pkgs = import <nixpkgs> {};
in ((pkgs.emacsPackagesFor pkgs.emacs).treesit-grammars.with-all-grammars)
'
```

Then symlink the tree-sitter libs into `~/.emacs.d/tree-sitter/`

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

## Nix profile packages

`nix profile list --json | jq '.elements | keys | .[]'`

```sh
"age"
"bat"
"direnv"
"fzf"
"gh"
"git"
"nix-direnv"
"nix-zsh-completions"
"nodejs"
"ripgrep"
"watch"
```
