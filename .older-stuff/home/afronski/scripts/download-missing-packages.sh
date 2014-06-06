#!/bin/bash

cache=`pacman -v 2>/dev/null|grep "Cache Dirs"|cut -d ':' -f 2| sed 's/ //g'`

for pkg in `comm -23 <(pacman -Qq|sort) <(pacman -Qmq|sort)`
do
        if ! ls $cache/`pacman -Q $pkg|sed 's/ /-/g'`* 2>/dev/null >&2;then
                echo 'Y'|pacman -Sw $pkg
        fi
done
