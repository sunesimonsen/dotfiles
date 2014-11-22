#!/bin/bash

SCREEN_DIR=~/Pictures/screenshots/
SCREEN_PROMPT=1

window=''

case $1 in
  root)
    window='root';;
  active)
    window=`xprop -root | grep "_NET_ACTIVE_WINDOW(WINDOW)" | cut -d' ' -f5`;;
  area)
    window='';;
esac

[ ! -z "$SCREEN_PROMPT" ] && \
  name=`i3-input -P 'screen-name: ' | sed -n '/command = /s/command = //p'`

if [ -z "$name" ];then
  if [ $window == "root" ];then
    name='root'
  else
    name=`xprop -id $window | sed -n '/WM_CLASS/s/.* = "\([^\"]*\)".*/\1\n/p'`
    [ -z "$name" ] && name='window'
  fi
fi

filename="$name-`date +%Y-%m-%d_%H-%M-%S`.png"

if [ -z "$window" ];then
    import "$SCREEN_DIR/$filename"
else
    import -border -window $window "$SCREEN_DIR/$filename"
fi

ln -sf "$filename" $SCREEN_DIR/last

exit
