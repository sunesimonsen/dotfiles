#!/bin/bash

USERID=`id -u`

i3status -c ~/.i3/i3status.conf | while :
do
    read line

    TIMECLOCK='['
    if [ -e /tmp/emacs$USERID/server ]
    then
        STATUS=`emacsclient --eval "(timeclock-status-string)" | sed 's/"//g'`
        COLOR="#00FF00"
        if [[ $STATUS == *"OUT"* ]]
        then
            COLOR="#FF0000"
        fi
        TIMECLOCK+='{"name":"timeclock","instance":"one","full_text":"'
        TIMECLOCK+=$STATUS
        TIMECLOCK+='","color":"'
        TIMECLOCK+=$COLOR
        TIMECLOCK+='"},'
    fi
    echo "${line/[/[$TIMECLOCK}" || exit 1
done
