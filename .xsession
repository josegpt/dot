#!/bin/sh

xset b off

case $(hostname) in
	picua) xrandr --output HDMI-A-0 --pos 0x0 \
		--output DisplayPort-2 --pos 1920x0 --rotate left
	;;
esac

exec ratpoison
