---
allowed_tools: ["Bash(git add:*)", "Bash(git commit:*)", "Bash(git status:*)", "Bash(git rev-parse:*)"]
description: Make a concise git commit on the current branch
---

Create a concise git commit on the current branch.

Steps:
1. Show current git status and git diff to understand the changes
2. Generate an appropriate commit message based on the changes
3. Commit with the generated message
4. Ask the user if they want to push the changes
