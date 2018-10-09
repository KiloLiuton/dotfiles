#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls -hN --color=auto --group-directories-first'

PS1='[\u@\h \W]\$ '

export PATH="~/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
