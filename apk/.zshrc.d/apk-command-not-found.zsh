#!/bin/zsh
# apk(8) from Alpine Linux command-not-found hook for zsh

command_not_found_handler() {
	local cmd="$1"
	local pkgs=(${(f)"$(apk list -P -- "cmd:$cmd" 2>/dev/null)"})

	if [[ -n "$pkgs" ]]; then
		echo "$cmd may be found in the following packages:"
		printf '  %s\n' "${pkgs[@]}"
	else
		echo "zsh: command not found: $cmd"
	fi 1>&2

	return 127
}
