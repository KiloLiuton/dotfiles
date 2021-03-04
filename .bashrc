# If not running interactively, don't do anything
[[ $- != *i* ]] && return

so() {
  if [ -f "$1" ]; then
    source "$1"
  else
    echo >&2 "warning: couldn't source $1, no such file or directory"
  fi
}

function logcommand()
{
    echo -e "#!/bin/bash\n$@" > cmdlog.sh
    chmod +x cmdlog.sh
    ./cmdlog.sh
}

function movetotrash()
{
    mv "$@" /home/kevin/.local/share/Trash/files
}

HISTCONTROL=ignoreboth
HISTSIZE=1000
HISTFILESIZE=2000
HISTIGNORE='clear'

alias del=movetotrash
alias mv='mv -i'
alias cp='cp -i'
alias ls='ls -hN --color=auto --group-directories-first'
alias grep='grep --color=auto'
alias icat='kitty +kitten icat'
alias vim='nvim'
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME'

source <(kitty + complete setup bash)
so /usr/share/git/git-prompt.sh
so /usr/share/git/completion/git-completion.bash
so /usr/share/bash-completion/bash_completion

export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1
export GIT_PS1_SHOWUPSTREAM=verbose

# Enable colors using tput and the colored PS1 to use Kevin's custom PS1
PURPLE=$(tput setaf 200)
LIME_YELLOW=$(tput setaf 190)
WHITE=$(tput setaf 7)
BRIGHT=$(tput bold)
NORMAL=$(tput sgr0)
BLINK=$(tput blink)
REVERSE=$(tput smso)
UNDERLINE=$(tput smul)

myprompt() {
  if [ -n "$SSH_CLIENT" ]; then
      __git_ps1 "[\\u@\[\e[32m\e[1m\]\\h\[\e[0m\]:\\W]" "\$ "
      #__git_ps1 "\[${BRIGHT}\]\[${WHITE}\][\[${LIME_YELLOW}\]\u\[${PURPLE}\]@\[${LIME_YELLOW}\]\h\[${BRIGHT}\]\[${WHITE}\] \W]\[${NORMAL}\]$ "
      #__git_ps1 "\[${BRIGHT}\]\[${BLINK}\]\\W" "\[${PURPLE}\]\$\[${NORMAL}\] "
  else
      __git_ps1 "\[${BRIGHT}\]\[${LIME_YELLOW}\]\\W" "\[${PURPLE}\]\$\[${NORMAL}\] "
      #__git_ps1 "\[${BRIGHT}\]\[${WHITE}\][\[${LIME_YELLOW}\]\u\[${PURPLE}\]@\[${LIME_YELLOW}\]\h\[${BRIGHT}\]\[${WHITE}\] \W]\[${NORMAL}\]$ "
  fi
}

# PS1="\[${BRIGHT}\]\[${WHITE}\][\[${LIME_YELLOW}\]\u\[${PURPLE}\]@\[${LIME_YELLOW}\]\h\[${BRIGHT}\]\[${WHITE}\] \W]\[${NORMAL}\]$ "
# PS1='[\u@\h \W]\$ '

# Uncomment bellow to load the the bash_prompt file if you want the rainbow bash prompt!
# export PATH="~/AUR/c-lolcat/src/c-lolcat/:$PATH"
# if [[ -f ~/.bash_prompt ]]; then
#   . ~/.bash_prompt
# fi

if [ -f /usr/share/git/completion/git-prompt.sh ]; then
    PROMPT_COMMAND=myprompt
else
    PS1="\[${BRIGHT}\]\[${WHITE}\][\[${LIME_YELLOW}\]\u\[${PURPLE}\]@\[${LIME_YELLOW}\]\h\[${BRIGHT}\]\[${WHITE}\] \W]\[${NORMAL}\]$ "
fi

export EDITOR=vim
export PATH="$HOME/bin:$PATH"
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$HOME/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
