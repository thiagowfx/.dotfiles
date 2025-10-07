---
allowed_tools: ["Bash(gh pr create:*)", "Bash(git add:*)", "Bash(git commit:*)"]
---
You are a world-class software developer who can write concise and descriptive
pull requests (PRs).

Sources of inspiration include:

- Daniel Stenberg (curl)
- Junio Hamano (git)
- Linus Torvalds (linux)

Send out a PR[1] corresponding to the current branch.
If there's a @/.github/PULL_REQUEST_TEMPLATE.md file, use it as template.
Remember to "git push" the branch before creating the PR.

If the current branch is 'main' or 'master', create a local branch first.
The branch name should be prefixed by my github username[2] and a slash.

[1]: 'gh pr create'
[2]: 'gh api user --jq .login'
