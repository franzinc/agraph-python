#!/bin/sh

if [ "$1" = start ]
then
    echo "monitor.sh: Starting monitoring for $2..."
    nmon -t -C agraph -F "$2.nmon" -s 10
elif [ "$1" = end ]
then
    echo "monitor.sh: Ending monitoring."
    killall -USR2 nmon

    dbname=events_test
    catalog=tests

    [ -n "${AGRAPH_LISP_CLIENT-}" ] &&
    [ -n "${AGRAPH_BENCH_DIR-}" ] &&
    [ -n "${AGRAPH_PORT-}" ] &&
    /fi/cl/8.2/agraph/bin/mlisp-64 \
	-L $AGRAPH_LISP_CLIENT/agraph4.fasl \
	-L $AGRAPH_BENCH_DIR/ensure-db-idle.cl \
	-e "(db.agraph.storage::synchronize \
               \"${dbname}\" \"${catalog}\" $AGRAPH_PORT)" \
	-kill
fi
