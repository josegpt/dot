#!/bin/sh

xhost +SI:localuser:$USER

exec emacs
