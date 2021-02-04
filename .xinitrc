#!/bin/bash
export XDG_CURRENT_DESKTOP=GNOME
export _JAVA_AWT_WM_NONREPARENING=1
export AWT_TOOLKIT=MToolkit
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=lcd'

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

/etc/X11/xinit/xinitrc.d/40-libcanberra-gtk-module.sh &
/etc/X11/xinit/xinitrc.d/50-systemd-user.sh &

#dbus-update-activation-environment --systemd DBUS_SESSION_BUS_ADDRESS DISPLAY XAUTHORITY &
#exec dbus-launch --exit-with-session xmonad
exec xmonad
#exec openbox-session
