#!/bin/bash

USERID=`id -u`

i3status -c ~/.i3/i3status.conf | while :
do
    read line

    TIMECLOCK='['
    if [ -e /tmp/emacs$USERID/server ]
    then
        TIMECLOCK+='{"name":"timeclock","instance":"one","full_text":"'
        TIMECLOCK+=`emacsclient --eval "(timeclock-status-string)" | sed 's/"//g'`
        TIMECLOCK+='"},'
    fi
    echo "${line/[/[$TIMECLOCK}" || exit 1
done
