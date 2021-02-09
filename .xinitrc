#!/bin/sh
export XDG_CURRENT_DESKTOP=GNOME
export _JAVA_AWT_WM_NONREPARENING=1
export AWT_TOOLKIT=MToolkit
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=lcd'

xrdb -merge ~/.Xresources

setxkbmap \
    -layout us,us \
    -variant ,intl \
    -option grp:win_space_toggle

xset r rate 300 70

xsetroot -cursor_name left_ptr

xrandr --output eDP1 --mode 1920x1080 --rate 60

nitrogen --set-auto --random ~/Pictures/wallpapers/

stalonetray &

picom &

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

exec xmonad
