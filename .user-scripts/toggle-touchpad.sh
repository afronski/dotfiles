#!/bin/bash

if [ "`synclient -l | grep TouchpadOff | awk '{print $3; }'`" == "1" ]; then
    synclient TouchpadOff=0
else
    synclient TouchpadOff=1
fi
