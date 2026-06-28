#!/bin/bash
# PreToolUse hook to block dangerous commands
# shellcheck disable=SC2154 # tool_name and command are assigned via eval

input=$(cat)

# Parse JSON once (variables assigned via eval)
eval "$(echo "$input" | jq -r '@sh "tool_name=\(.tool_name // "") command=\(.tool_input.command // "")"')"

# Exit early if not a Bash tool
[[ $tool_name != "Bash" ]] && exit 0

# Define blocked patterns and reasons (parallel arrays)
# Note: Using [[:space:]] instead of \s for POSIX ERE compatibility
patterns=(
    'rm[[:space:]]+(-[a-zA-Z]*r[a-zA-Z]*[[:space:]]+-[a-zA-Z]*f|-[a-zA-Z]*f[a-zA-Z]*[[:space:]]+-[a-zA-Z]*r|-[rRf]*r[rRf]*f|-[rRf]*f[rRf]*r)'
    'terraform[[:space:]]+apply'
    'terraform[[:space:]]+destroy'
    '--auto-approve'
    'just[[:space:]]+apply'
    'just[[:space:]]+destroy'
    'rm[[:space:]]+.*\.cache/pre-commit'
    'git[[:space:]]+push[[:space:]].*(--force|--force-with-lease|-[a-zA-Z]*f)[[:space:]].*(master|main)'
    'git[[:space:]]+push[[:space:]].*(master|main).*(--force|--force-with-lease|-f)'
    'git[[:space:]]+reset[[:space:]]+.*--hard'
    'git[[:space:]]+clean[[:space:]]+.*-[a-zA-Z]*[fdx]'
    'git[[:space:]]+(commit|push).*--no-verify'
)
reasons=(
    "rm -rf is blocked for safety"
    "terraform apply is blocked - use terraform plan first"
    "terraform destroy is blocked for safety"
    "--auto-approve is blocked - manual confirmation required"
    "just apply is blocked - use just plan first"
    "just destroy is blocked for safety"
    "Deleting pre-commit cache is blocked for safety"
    "Force-push to master/main is blocked"
    "Force-push to master/main is blocked"
    "git reset --hard is blocked - discards changes irreversibly"
    "git clean is blocked - deletes untracked files irreversibly"
    "--no-verify is blocked - run the hooks"
)

# Check all patterns
for i in "${!patterns[@]}"; do
    if [[ $command =~ ${patterns[$i]} ]]; then
        jq -n --arg reason "${reasons[$i]}" '{
            hookSpecificOutput: {
                hookEventName: "PreToolUse",
                permissionDecision: "deny",
                permissionDecisionReason: $reason
            }
        }'
        exit 0
    fi
done

exit 0
