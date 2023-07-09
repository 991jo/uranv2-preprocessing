#!/bin/bash

measure_memory() {
	echo $PID
	STATUS=/proc/$PID/status
	MAX_MEMORY=0

	while [ -e "$STATUS" ]; do
		MEMORY=$(grep "VmRSS" $STATUS | cut -d ":" -f2- | sed -e 's/^[[:space:]]*//' | cut -d" " -f1)
		MAX_MEMORY=$(( MEMORY > MAX_MEMORY ? MEMORY : MAX_MEMORY ))

		sleep 0.01
	done

	echo $MAX_MEMORY
}

for method in naive iterative efficient
do
	cargo run --release --bin=packing -- -g ../data/stgt.sch -p $method -o stgt-$method-error.ranges 2>&1 >> /dev/null &
	PID=$!
	measure_memory

done;



