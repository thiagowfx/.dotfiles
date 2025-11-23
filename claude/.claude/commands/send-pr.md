---
allowed_tools: ["Bash(cat:*)", "Bash(gh api user:*)", "Bash(gh pr create:*)", "Bash(git add:*)", "Bash(git checkout:*)", "Bash(git commit:*)", "Bash(git push:*)", "Bash(git rev-parse:*)", "Bash(test:*)"]
argument-hint: [reviewer]
description: Send out a concise PR for review
---

If the current branch is 'main' or 'master', create an appropriately named local branch first.
The branch name should be prefixed by my github username and a slash (e.g."user/my-branch").

The username:
! gh api user --jq .login

Ensure commits were created.

Then push the branch:
! git push

Afterwards send out a PR for it, setting $ARGUMENTS as reviewers.
If there's a .github/PULL_REQUEST_TEMPLATE.md file at the git repository root, use it as template for the PR description.
To find the git repository root, use: git rev-parse --show-toplevel
Then check if the template file exists at $(git rev-parse --show-toplevel)/.github/PULL_REQUEST_TEMPLATE.md
! gh pr create

If a PR already exists, then update its description.
