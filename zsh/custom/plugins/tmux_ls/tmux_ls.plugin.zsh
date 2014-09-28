# function running_sessions() {
#   sessions=$(tmux ls 2> /dev/null) || return
#   echo ${sessions}
# }
# 
COMPREPLY=( $(tmux ls 2> /dev/null) )
# these aliases take advantage of the previous function
alias attach='tmux attach -t $(COMPREPLY)'
