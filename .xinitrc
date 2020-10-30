#!/bin/bash

xrdb -merge .Xresources

setxkbmap -layout us,us -variant ,intl -option grp:ctrls_toggle

xrandr --output DP-4 --mode 2560x1440 --rate 144

xset r rate 300 70

nitrogen --set-auto --random ~/Pictures/wallpapers/ &

xcompmgr -c &

stalonetray &

dbus-update-activation-environment --systemd DBUS_SESSION_BUS_ADDRESS DISPLAY XAUTHORITY &

exec dbus-launch --exit-with-session xmonad
