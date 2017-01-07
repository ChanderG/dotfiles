#!/bin/sh

# settings for urxvt
xrdb -merge .Xresources
urxvtd -q -o -f

# max redshift all the time
redshift -O 3700K

# run syspom - homegrown pomodoro timer with analytics dashboard
flock -n /tmp/syspom.lockfile syspom &>/dev/null &

# use modifiers on their own as well
# mapping here mean
#  left ctrl -> left_alt + o -> tdrop
#  windows key -> alt+shift+u -> new terminal
flock -n /tmp/xcape.lockfile xcape -e 'Control_L=Alt_L|o;Super_L=Alt_L|Shift_L|u'

# set the background
sh ~/.fehbg &

# start dwm
exec dwm
