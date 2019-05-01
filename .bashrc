#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

function movetotrash()
{
    mv "$@" /home/kevin/.local/share/Trash/files
}
alias del=movetotrash
alias mv='mv -i'
alias cp='cp -i'
alias ls='ls -hN --color=auto --group-directories-first'
alias icat='kitty +kitten icat'
alias vim='nvim'
source <(kitty + complete setup bash)

# Enable colors using tput and the colored PS1 to use Kevin's custom PS1
PURPLE=$(tput setaf 200)
LIME_YELLOW=$(tput setaf 190)
WHITE=$(tput setaf 7)
BRIGHT=$(tput bold)
NORMAL=$(tput sgr0)
BLINK=$(tput blink)
REVERSE=$(tput smso)
UNDERLINE=$(tput smul)
PS1="\[${BRIGHT}\]\[${WHITE}\][\[${LIME_YELLOW}\]\u\[${PURPLE}\]@\[${LIME_YELLOW}\]\h\[${BRIGHT}\]\[${WHITE}\] \W]\[${NORMAL}\]$ "
# PS1='[\u@\h \W]\$ '

# Uncomment bellow to load the the bash_prompt file
# if you want the rainbow bash prompt!

# export PATH="~/AUR/c-lolcat/src/c-lolcat/:$PATH"
# if [[ -f ~/.bash_prompt ]]; then
#   . ~/.bash_prompt
# fi

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$HOME/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
