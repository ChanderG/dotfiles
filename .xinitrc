#!/bin/sh

# settings for urxvt
xrdb -merge .Xresources
flock -n /tmp/urxvtd.lockfile urxvtd -q -o -f

# max redshift all the time
redshift -O 3700K

# run syspom - homegrown pomodoro timer with analytics dashboard
flock -n /tmp/syspom.lockfile syspom &>/dev/null &

# setup capslock as an additional escape; mainly for use as emacs meta
xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "keysym Caps_Lock = Escape"

# use modifiers on their own as well
# mapping here mean
#  left ctrl -> left_alt + o -> tdrop
#  windows key -> alt+shift+u -> new terminal
#  right alt -> left_alt + left -> go to prev dwm tag
#  right ctrl -> left_alt + right -> go to next dwm tag
#  Print Screen -> left_alt + 8 -> go to 8th tag
#  right shift -> left_alt + tab -> go to previous tag
flock -n /tmp/xcape.lockfile xcape -e 'Control_L=Alt_L|o;Super_L=Alt_L|Shift_L|u;Control_R=Alt_L|Right;Alt_R=Alt_L|Left;Print=Alt_L|8;Shift_R=Alt_L|Tab'

# set the background
sh ~/.fehbg &

# stop global mpd and start local mpd
sudo service stop mpd
[ ! -s ~/.config/mpd/pid ] && mpd

# transparency manager
flock -n /tmp/xcompmgr.lockfile xcompmgr &

# start dwm
exec dwm
