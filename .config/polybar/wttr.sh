#!/bin/sh

WEATHER=$(curl -s wttr.in/Clifton?format=4)

echo "$WEATHER"
