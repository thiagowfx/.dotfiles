#!/bin/bash
# PreToolUse hook to block dangerous commands like "rm -rf" and "terraform apply"

# Read hook input from stdin
input=$(cat)

# Extract tool name and command
tool_name=$(echo "$input" | jq -r '.tool_name // ""')
command=$(echo "$input" | jq -r '.tool_input.command // ""')

# Exit early if not a Bash tool
if [ "$tool_name" != "Bash" ]; then
    exit 0
fi

# Check for dangerous patterns
blocked=false
reason=""

# Check for rm -rf (with various flag orderings)
if echo "$command" | grep -qE 'rm\s+(-[a-zA-Z]*r[a-zA-Z]*f|(-[a-zA-Z]*f[a-zA-Z]*\s+)?-[a-zA-Z]*r|-rf|-fr)\b'; then
    blocked=true
    reason="rm -rf is blocked for safety"
fi

# Check for terraform apply (without -auto-approve is still dangerous)
if echo "$command" | grep -qE 'terraform\s+apply'; then
    blocked=true
    reason="terraform apply is blocked - use terraform plan first"
fi

# Check for terraform destroy
if echo "$command" | grep -qE 'terraform\s+destroy'; then
    blocked=true
    reason="terraform destroy is blocked for safety"
fi

# If blocked, return denial via JSON
if [ "$blocked" = true ]; then
    cat <<EOF
{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": "$reason"
  }
}
EOF
    exit 0
fi

# Allow command to proceed
exit 0
