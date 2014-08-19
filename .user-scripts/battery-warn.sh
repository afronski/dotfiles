#!/bin/bash

DIR="/sys/class/power_supply/BAT0"
STATUS=$(cat "$DIR/status")

if [ "$STATUS" == "Discharging" ]
then
        for PREFIX in "charge" "energy"
        do
                if [ -f "$DIR/${PREFIX}_now" ] && [ -f "$DIR/${PREFIX}_full" ]
                then
                        REMAIN=$(cat "$DIR/${PREFIX}_now")
                        FULL=$(cat "$DIR/${PREFIX}_full")
                        PRCT=$((100 * $REMAIN / $FULL))

                        if [ $PRCT -le 10 ]
                        then
                                notify-send -t 5000 -u critical "Battery low... (10%)"
                        fi
                        break
                fi
        done
fi
