#!/bin/sh

xrdb -merge .Xresources

urxvtd -q -o -f
exec dwm
