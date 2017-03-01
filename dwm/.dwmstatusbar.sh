#!/bin/sh
while true; do
	xsetroot -name "$(acpi | cut -d' ' -f3-) | $(date +%H:%M) | $(whoami)"
	sleep 5
done
