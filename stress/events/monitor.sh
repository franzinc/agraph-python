#!/bin/sh

if [ "$1" = start ]
then
    echo "monitor.sh: Starting monitoring for $2..."
    nmon -C agraph -F "$2.nmon" -s 10
elif [ "$1" = end ]
then
    echo "monitor.sh: Ending monitoring."
    killall -USR2 nmon
fi
