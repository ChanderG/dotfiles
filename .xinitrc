#!/bin/sh

# settings for urxvt
xrdb -merge .Xresources
urxvtd -q -o -f

# max redshift all the time
redshift -O 3700K

# run syspom - homegrown pomodoro timer with analytics dashboard
syspom &>/dev/null &

# start dwm
exec dwm
