#!/bin/bash
# apk(8) from Alpine Linux command-not-found hook for bash

command_not_found_handle () {
	local cmd="$1" pkgs
	mapfile -t pkgs < <(apk list -P -- "cmd:$cmd" 2>/dev/null)

	if (( ${#pkgs[*]} )); then
		echo "$cmd may be found in the following packages:"
		printf '  %s\n' "${pkgs[@]}"
	else
		echo "bash: command not found: $cmd"
	fi 1>&2

	return 127
}
