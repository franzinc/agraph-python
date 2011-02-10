#!/bin/sh

if [ "$1" = start ]
then
    echo "monitor.sh: Starting monitoring for $2..."
    nmon -t -C agraph -F "$2.nmon" -s 10
elif [ "$1" = end ]
then
    echo "monitor.sh: Ending monitoring."
    killall -USR2 nmon

    catalog=$3
    dbname=$4

    [ -n "${AGRAPH_LISP_CLIENT-}" ] &&
    [ -f /fi/cl/8.2/agraph/bin/mlisp-64 ] &&
    echo "[" `date --rfc-3339=ns` "] Phase 0 Begin: (sync)" &&
    /fi/cl/8.2/agraph/bin/mlisp-64 \
	-L $AGRAPH_LISP_CLIENT/agraph4.fasl \
	-e "(db.agraph.storage::sl-ensure-db-idle \
               \"${dbname}\" \"${catalog}\" ${AGRAPH_PORT:-nil})" \
	-kill &&
    echo "[" `date --rfc-3339=ns` "] Phase 0 End: (sync)."
fi
