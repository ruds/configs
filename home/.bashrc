if [ -f /etc/bash.bashrc ]; then
  . /etc/bash.bashrc
fi

if [ -f $HOME/.bash_noninteractive ]; then
  . $HOME/.bash_noninteractive
  export BASH_ENV=$HOME/.bash_noninteractive
else
  if [ -f $HOME/.aliases ]; then
    . $HOME/.aliases
  fi
  if [ -f $HOME/.bash_functions ]; then
    . $HOME/.bash_functions
  fi
fi

ulimit -c 200000

export HISTIGNORE="&:ls:[bf]g:exit"
export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=1000000
export HISTFILESIZE=1000000
export HISTTIMEFORMAT='%G %T '
export LANG=C

shopt -s checkwinsize
shopt -s histappend

export PROMPT_COMMAND="history -a; history -n; $PROMPT_COMMAND"

export EDITOR=vim

case $TERM in
  xterm*)
    if [[ "x$EMACS" == "x" ]]; then
      alias ls='ls -F --color=tty'
      PS1_base="\[\033]0;\h: \w\007\] [\t] \W\\$ "
    else
      alias ls='ls -F'
    fi
    ;;
  *)
    alias ls='ls -F'
    PS1_base="[\t] \W @\h\\$ "
    ;;
esac

export PS1_base
export PS1=$PS1_base

export MANOPT="-E latin1"

source $HOME/.git-completion.bash
