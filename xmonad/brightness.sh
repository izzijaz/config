#!/bin/bash

monitor="eDP-1-1"
current_brightness=$(xrandr --verbose --current | awk -F '[ /]' '/Brightness/ {print $2}')
increment=$1

# Check if argument is provided
if [ -z "$increment" ]; then
  echo "Usage: ./brightness.sh <increment>"
  echo "Please provide an increment value."
  exit 1
fi

# Calculate new brightness
new_brightness=$(awk -v current="$current_brightness" -v inc="$increment" 'BEGIN {print current + inc}')
if (( $(echo "$new_brightness > 1.0" | bc -l) )); then
  new_brightness=1.0
elif (( $(echo "$new_brightness < 0.0" | bc -l) )); then
  new_brightness=0.0
fi

# Set new brightness using xrandr
xrandr --output "$monitor" --brightness "$new_brightness"

