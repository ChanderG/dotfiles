#!/bin/sh

xrdb -merge .Xresources

exec dwm
