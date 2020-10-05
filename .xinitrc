setxkbmap \
    -layout us,us \
    -variant ,intl \
    -option grp:win_space_toggle

xset r rate 300 70

nitrogen --set-auto --random ~/Pictures/wallpapers/

xcompmgr -c &

exec xmonad

