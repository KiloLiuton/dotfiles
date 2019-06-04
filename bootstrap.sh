#!/bin/bash
function config {
   /usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME $@
}

# if the config folder does not exist, clone it from github
if [ ! -d $HOME/.cfg ]; then
    git clone --bare --single-branch -b $1 git@github.com:KiloLiuton/dotfiles.git $HOME/.cfg
else
    echo -n "The folder $HOME/.cfg already exists, continue? [Y/n] "
    read answer
    case "$answer" in
        y|Y|'')
            echo 'Continuing...'
            ;;
        *)
            exit 1
            ;;
    esac
fi

echo -n "Do you want to backup existing dot files? [y/N] "
read answer

case "$answer" in
    y|Y)
        mkdir -p $HOME/.config-backup
        for f in $(config checkout 2>&1 | egrep "\s+\." | awk {'print $1'});
        do
            echo mv $HOME/$f $HOME/.config-backup/
            mv $HOME/$f $HOME/.config-backup/
        done
        echo config checkout
        config checkout
        ;;
    *)
        for f in $(config checkout 2>&1 | egrep "\s+\." | awk {'print $1'});
        do
            echo rm $HOME/$f
            rm $HOME/$f
        done
        echo config checkout
        config checkout
        ;;
esac

echo config config status.showUntrackedFiles no
config config status.showUntrackedFiles no

echo source $HOME/.bashrc
source $HOME/.bashrc
