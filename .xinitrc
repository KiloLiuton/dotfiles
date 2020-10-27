setxkbmap \
    -layout us,us \
    -variant ,intl \
    -option grp:win_space_toggle

xset r rate 300 70

nitrogen --set-auto --random ~/Pictures/wallpapers/

xcompmgr -c &

dbus-update-activation-environment \
	--systemd DBUS_SESSION_BUS_ADDRESS DISPLAY XAUTHORITY &

exec dbus-launch --exit-with-session xmonad

