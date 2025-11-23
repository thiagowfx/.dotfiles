---
allowed_tools: ["Bash(git add:*)", "Bash(git commit:*)", "Bash(git status:*)", "Bash(git rev-parse:*)"]
argument-hint: [message]
description: Make a concise git commit on the current branch
---

Create a concise git commit with the provided message.

Steps:
1. Show current git status to confirm changes
2. Commit with the provided message
3. Ask the user if they want to push the changes

If the user provides a message as argument, use it directly. Otherwise, ask them for a commit message.
