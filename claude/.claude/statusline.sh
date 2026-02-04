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
            dirty="*"
        else
            dirty=""
        fi

        # Check if in a linked worktree (not the main worktree)
        worktree_info=""
        git_dir=$(git rev-parse --git-dir 2>/dev/null)
        git_common_dir=$(git rev-parse --git-common-dir 2>/dev/null)
        if [[ "$git_dir" != "$git_common_dir" ]]; then
            # We're in a linked worktree - show its name
            worktree_path=$(git rev-parse --show-toplevel 2>/dev/null)
            worktree_name=$(basename "$worktree_path")
            worktree_info="⎇$worktree_name"
        fi

        git_info="(git:$branch$worktree_info$dirty)"
    fi
fi

# Calculate context window usage (relative to auto-compact threshold ~80%)
context_info=""
usage=$(echo "$input" | jq '.context_window.current_usage')
if [ "$usage" != "null" ]; then
    # Include output_tokens for accurate context measurement
    current=$(echo "$usage" | jq '.input_tokens + .output_tokens + .cache_creation_input_tokens + .cache_read_input_tokens')
    size=$(echo "$input" | jq '.context_window.context_window_size')

    # Auto-compact triggers at ~80% of context window
    compact_threshold=$((size * 80 / 100))

    # Calculate percentage toward auto-compact (capped at 100%)
    pct=$((current * 100 / compact_threshold))
    if [ "$pct" -gt 100 ]; then pct=100; fi

    # Create progress bar (5 characters wide)
    filled=$((pct / 20))
    empty=$((5 - filled))
    bar=""
    for ((i=0; i<filled; i++)); do bar+="█"; done
    for ((i=0; i<empty; i++)); do bar+="░"; done

    context_info="[${bar}]"
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

# Add context usage if available
if [[ -n "$context_info" ]]; then
    status_parts+=("$context_info")
fi

# Join parts with spaces and print
printf "%s" "$(IFS=' '; echo "${status_parts[*]}")"
