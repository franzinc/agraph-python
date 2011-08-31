#!/bin/sh

if [ "$1" = start ]
then
    echo "monitor.sh: Starting monitoring for $2..."
    nmon -t -C agraph -F "$2.nmon" -s 10
    cat /proc/diskstats > $2.ds
    echo "-------" >> $2.ds
    cat /proc/vmstat > $2.vm
    echo "-------" >> $2.vm
elif [ "$1" = end ]
then
    echo "monitor.sh: Ending monitoring."
    cat /proc/diskstats >> $2.ds
    cat /proc/vmstat >> $2.vm
    killall -USR2 nmon

fi
