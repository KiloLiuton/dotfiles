#!/bin/bash
function config {
   /usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME $@
}

# if the config folder does not exist, clone it from github
if [ ! -d $HOME/.cfg ]; then
    git clone --bare --single-branch -b $1 git@github.com:KiloLiuton/dotfiles.git $HOME/.cfg
    # git clone --bare git@github.com:KiloLiuton/dotfiles.git $HOME/.cfg
else
    echo -n "A ~/.cfg folder already exists! Continue? [(y)/n] "
    read answer
    case "$answer" in
        y|Y|'') ;;
        *) exit 1 ;;
    esac
fi

echo -n "Do you want to backup existing dot files? [(y)/n] "
read answer

case "$answer" in
    y|Y|'')
        mkdir -p .config-backup
        echo "Backing up existing dot files to ~/.config-backup";
        config checkout 2>&1 | egrep "\s+\." | awk {'print $1'} | xargs -I{} mv {} .config-backup/{}
        config checkout
        echo "Checked out config.";
        ;;
    *)
        echo "Removing existing dot files"
        config checkout 2>&1 | egrep "\s+\." | awk {'print $1'} | xargs -I{} rm {}
        config checkout
        echo "Checked out config.";
        ;;
esac

config config status.showUntrackedFiles no
source $HOME/.bashrc
