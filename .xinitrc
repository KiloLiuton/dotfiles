#!/bin/bash

xrdb -merge .Xresources

setxkbmap -layout us,us -variant ,intl -option grp:ctrls_toggle

xset r rate 300 70

nitrogen --set-auto --random ~/Pictures/wallpapers/ &

xcompmgr -c &

exec xmonad

