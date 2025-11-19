---
allowed_tools: ["Bash(cat:*)", "Bash(gh api user:*)", "Bash(gh pr create:*)", "Bash(git add:*)", "Bash(git checkout:*)", "Bash(git commit:*)", "Bash(git push:*)", "Bash(git rev-parse:*)", "Bash(test:*)"]
argument-hint: [reviewer]
description: Make a concise git commit
---

Use the current branch.

Commit with --no-verify, to bypass pre-commit hooks.

If there's a .github/PULL_REQUEST_TEMPLATE.md file at the git repository root, use it as template for the PR description.
To find the git repository root, use: git rev-parse --show-toplevel
Then check if the template file exists at $(git rev-parse --show-toplevel)/.github/PULL_REQUEST_TEMPLATE.md
