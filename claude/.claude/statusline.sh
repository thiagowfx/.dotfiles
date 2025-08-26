#!/bin/bash

# Read JSON input from stdin
input=$(cat)

# Extract information from JSON
model_name=$(echo "$input" | jq -r '.model.display_name')
current_dir=$(echo "$input" | jq -r '.workspace.current_dir')
output_style=$(echo "$input" | jq -r '.output_style.name')

# Get current working directory basename
dir_name=$(basename "$current_dir")

# Get git information if in a git repository
git_info=""
if git rev-parse --git-dir > /dev/null 2>&1; then
    branch=$(git branch --show-current 2>/dev/null)
    if [[ -n "$branch" ]]; then
        # Check for uncommitted changes
        if ! git diff --quiet 2>/dev/null || ! git diff --cached --quiet 2>/dev/null; then
            git_info="(git:$branch*)"
        else
            git_info="(git:$branch)"
        fi
    fi
fi

# Build status line
status_parts=()

# Add current directory
status_parts+=("$dir_name")

# Add git info if available
if [[ -n "$git_info" ]]; then
    status_parts+=("$git_info")
fi

# Add model name
status_parts+=("[$model_name]")

# Add output style if not default
if [[ "$output_style" != "default" && "$output_style" != "null" ]]; then
    status_parts+=("{$output_style}")
fi

# Join parts with spaces and print
printf "%s" "$(IFS=' '; echo "${status_parts[*]}")"