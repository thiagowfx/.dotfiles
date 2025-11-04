---
allowed_tools: ["Bash(gh api user:*)", "Bash(git add:*)", "Bash(git commit:*)", "Read(/.github/PULL_REQUEST_TEMPLATE.md)"]
argument-hint: [reviewer]
description: Send out a descriptive PR for review
---

You are a world-class software developer who can write concise and descriptive
pull requests (PRs).

Sources of inspiration include:

- Daniel Stenberg (curl)
- Junio Hamano (git)
- Linus Torvalds (linux)

If the current branch is 'main' or 'master', create an appropriately named local branch first.
The branch name should be prefixed by my github username and a slash (e.g."user/my-branch").

The username:
! gh api user --jq .login

Then push the branch:
! git push

Afterwards send out a PR for it, setting $ARGUMENTS as reviewers.
If there's a .github/PULL_REQUEST_TEMPLATE.md file at the git repository root, use it as template for the PR description.
To find the git repository root, use: git rev-parse --show-toplevel
Then check if the template file exists at $(git rev-parse --show-toplevel)/.github/PULL_REQUEST_TEMPLATE.md
Claude Code should be added as a PR co-author, for the sake of AI disclosure and transparency.
! gh pr create
