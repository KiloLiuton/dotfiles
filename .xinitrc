#!/bin/bash

xrdb -merge .Xresources

setxkbmap -layout us,us -variant ,intl -option grp:ctrls_toggle

xrandr --output DP-0 --mode 2560x1440 --rate 144

# increase/decrease keyboard repeat rate/repeat delay
xset r rate 300 70

xsetroot --cursor_name left_ptr # for enabling mouse cursor on startup, currently not working

nitrogen --set-auto --random ~/Pictures/wallpapers/ &

picom &

stalonetray &

ckb-next --background &  # for lightining corsair peripherals

#dbus-update-activation-environment --systemd DBUS_SESSION_BUS_ADDRESS DISPLAY XAUTHORITY &
#exec dbus-launch --exit-with-session xmonad

exec xmonad
#exec openbox-session
