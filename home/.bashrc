alias ls='ls -F'
export EDITOR=vim

export HISTSIZE=1000000
export HISTFILESIZE=1000000
shopt -s histappend

export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"

PS1="\u@\h \W$ "
