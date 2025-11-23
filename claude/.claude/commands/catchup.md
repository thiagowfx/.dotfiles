---
description: Inspect all uncommitted changes in the repository using git diff
argument-hint: Optional - provide a file path to see changes for specific file(s)
allowed-tools: Bash(git status:*), Bash(git diff:*), Bash(git log:*)
---

I want to catch up on all the changes in this repository since the last commit.

Please show me:
1. Current repository status with `git status`
2. Detailed diff of all uncommitted changes with `git diff`

Execute these commands:
- !`git status`
- !`git diff`

$ARGUMENTS

Then provide a concise summary of:
- Which files have been modified
- Key changes made (functions added/removed, logic changes)
- Any staged vs unstaged changes
- Whether any files are untracked
