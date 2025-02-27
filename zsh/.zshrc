# Fix for TRAMP
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return
#http://stackoverflow.com/a/12575883/2344737
autoload -Uz promptinit && promptinit
autoload -Uz compinit && compinit
#https://wiki.archlinux.org/index.php/zsh#Prompts
autoload -Uz colors && colors

if [ -n "$INSIDE_EMACS" ]; then
  export TERM=xterm-256color
fi

#http://zsh.sourceforge.net/Doc/Release/Options.html
#https://martinheinz.dev/blog/110
SAVEHIST=10000000
HISTSIZE=10000000
HISTFILE=~/.zsh_history
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt EXTENDED_HISTORY
setopt HIST_SAVE_NO_DUPS
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_FIND_NO_DUPS
setopt correctall

if [ -f ~/.bash_profile ]; then
  source ~/.bash_profile
fi

# export GOPATH=$HOME/go
# export GOROOT=/Users/asimpson/Projects
# export PATH="$PATH:/usr/local/go/bin/:$GOPATH/bin"
# PATH="$PATH:$GOROOT"

precmd() {
  gitStatus="$(git symbolic-ref --short -q HEAD 2> /dev/null)"
  NEWLINE=$'\n'

  PROMPT="%~ %{$fg[yellow]%}${gitStatus}${NEWLINE}%D{%I:%M}%{$fg[magenta]%}❯ %{$reset_color%}"
}

source ~/.dotfiles/zsh/plugins/ht.plugin.zsh

export EDITOR='vim '

# Use C-x C-e to edit the current command line
# http://nuclearsquid.com/writings/edit-long-commands/
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line

#http://superuser.com/questions/417627/oh-my-zsh-history-completion
bindkey '\e[A' history-beginning-search-backward
bindkey '\e[B' history-beginning-search-forward

export PATH="$HOME/.yarn/bin:$PATH"

export PATH="$HOME/bin:$PATH"

# ssh-add -A 2>/dev/null;

autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /usr/local/bin/terraform terraform

if [ -e /home/asimpson/.nix-profile/etc/profile.d/nix.sh ]; then . /home/asimpson/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
eval "$(direnv hook zsh)"

eval "$(fzf --zsh)"

select_cluster() {
    local clusters=$(kubectl config get-contexts -o name)
    echo "$clusters" | fzf --height 40% --border --prompt="Select cluster: " --layout=reverse
}

insert_cluster() {
    local selected=$(select_cluster)
    if [ -n "$selected" ]; then
        LBUFFER+="$selected"
    fi
    zle redisplay
}

zle -N insert_cluster
bindkey '^k' insert_cluster

# Optional helper for command substitution
get_cluster() {
    select_cluster
}
