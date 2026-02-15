---
allowed_tools: ["Bash(cat:*)", "Bash(gh api user:*)", "Bash(gh pr create:*)", "Bash(gh pr edit:*)", "Bash(gh pr view:*)", "Bash(git add:*)", "Bash(git checkout:*)", "Bash(git commit:*)", "Bash(git diff:*)", "Bash(git log:*)", "Bash(git push:*)", "Bash(git rev-parse:*)", "Bash(git status:*)", "Bash(test:*)"]
argument-hint: [reviewer]
description: Commit changes and send a PR for review
---

Commit all changes and create a pull request. Follow these steps:

## Gather Context

! gh api user --jq .login
! git rev-parse --show-toplevel
! git rev-parse --abbrev-ref HEAD
! git status
! git diff
! git diff --staged

## Step 1: Commit Changes

- Stage all changes: `git add -A`
- Generate a conventional commit message based on the changes (e.g., `feat:`, `fix:`, `refactor:`, `docs:`, `chore:`)
- Create the commit with the generated message
- If pre-commit hooks fail due to environment issues (not code issues), retry with `--no-verify`
- If there are no changes to commit, skip to Step 2

## Step 2: Verify Prerequisites

- Run `git log origin/main..HEAD --oneline` (if not main, then master)
- Check that there are commits to push
- If no commits exist, stop and inform the user: "No commits to push. Create commits first."

## Step 3: Handle Branch Creation

- If current branch is 'main' or 'master', create a new branch first:
  - Generate a descriptive branch name based on the changes
  - Prefix it with the GitHub username and slash (e.g., "username/feature-name")
  - Use `git checkout -b <branch-name>` to create and switch to it

## Step 4: Push the Branch

- Push with upstream tracking: `git push -u origin <branch-name>`
- If push fails, report the error and stop

## Step 5: Check for Existing PR

- Run `gh pr view --json url,state` to check if a PR already exists for this branch
- If a PR exists and state is "OPEN":
  - Output: "Pushed to existing PR: {url}"
  - Stop here, do not continue to Step 6
- If a PR exists but state is "MERGED" or "CLOSED", continue to Step 6 (create a new PR)
- If no PR exists, continue to Step 6

## Step 6: Check for PR Template

- Check for PR template in these locations (in order of priority):
  1. `.github/PULL_REQUEST_TEMPLATE.md`
  2. `PULL_REQUEST_TEMPLATE.md` (repository root)
  3. `docs/PULL_REQUEST_TEMPLATE.md`
- Use `test -f <path>` to check existence, then `cat` to read the first one found
- If a template exists, you MUST use it as the structure for the PR description

## Step 7: Create PR

- Analyze all commits and changes to create a concise PR description
- If a template was found in Step 6, you MUST follow its structure and fill in all sections
- Create PR with reviewers: `gh pr create --title "<title>" --body "<description>" --reviewer $ARGUMENTS`
- If $ARGUMENTS is empty, omit the --reviewer flag
- Output: "Created new PR: {url}"
