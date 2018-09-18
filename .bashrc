#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return


export LSCOLORS="Exgxcxdxbxegedabagacad"
alias ls='ls -lGH'

PS1='[\u@\h \W]\$ '

export PATH="~/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
