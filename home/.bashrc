alias emacs='/opt/local/bin/emacs'
alias emacsclient='/opt/local/bin/emacsclient'
alias ls='ls -F'
export EDITOR=vim

export HISTIGNORE="&:ls:[bf]g:exit"
export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=1000000
export HISTFILESIZE=1000000

shopt -s checkwinsize
shopt -s histappend

export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export LC_COLLATE=C

export PROMPT_COMMAND="history -a; history -n; $PROMPT_COMMAND"

PS1="\u@\h \W$ "

export MANOPT="-E latin1"

if [ -f $HOME/.git-completion.bash ]; then
  . $HOME/.git-completion.bash
fi
