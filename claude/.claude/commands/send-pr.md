---
allowed_tools: ["Bash(cat:*)", "Bash(gh api user:*)", "Bash(gh pr create:*)", "Bash(gh pr edit:*)", "Bash(gh pr view:*)", "Bash(git checkout:*)", "Bash(git diff:*)", "Bash(git log:*)", "Bash(git push:*)", "Bash(git rev-parse:*)", "Bash(git status:*)", "Bash(test:*)"]
argument-hint: [reviewer]
description: Send out a concise PR for review
---

Create and send a pull request for review. Follow these steps:

Gather context first (if not main, then master)

! gh api user --jq .login
! git rev-parse --show-toplevel
! git rev-parse --abbrev-ref HEAD
! git status
! git log origin/main..HEAD --oneline
! git diff origin/main...HEAD

### Step 1: Verify Prerequisites

- Check the git status output above for uncommitted changes
- If there are uncommitted changes, stop and inform the user: "You have uncommitted changes. Run /commit first or use /commit-and-pr."
- Check that there are commits to push (from git log output above)
- If no commits exist, stop and inform the user: "No commits to push. Create commits first."

### Step 2: Handle Branch Creation

- If current branch is 'main' or 'master', create a new branch first:
  - Generate a descriptive branch name based on the changes (from git diff above)
  - Prefix it with the GitHub username and slash (e.g., "username/feature-name")
  - Use `git checkout -b <branch-name>` to create and switch to it

### Step 3: Push the Branch

- Push with upstream tracking: `git push -u origin <branch-name>`
- If push fails, report the error and stop

### Step 4: Check for Existing PR

- Run `gh pr view --json url,state` to check if a PR already exists for this branch
- If a PR exists and state is "OPEN":
  - Output: "Pushed to existing PR: {url}"
  - Stop here, do not continue to Step 5
- If a PR exists but state is "MERGED" or "CLOSED", continue to Step 5 (create a new PR)
- If no PR exists, continue to Step 5

### Step 5: Check for PR Template

- Check for PR template in these locations (in order of priority):
  1. `.github/PULL_REQUEST_TEMPLATE.md`
  2. `PULL_REQUEST_TEMPLATE.md` (repository root)
  3. `docs/PULL_REQUEST_TEMPLATE.md`
- Use `test -f <path>` to check existence, then `cat` to read the first one found
- If a template exists, you MUST use it as the structure for the PR description

### Step 6: Create PR

- Analyze all commits and changes to create a concise PR description
- If a template was found in Step 5, you MUST follow its structure and fill in all sections
- Create PR with reviewers: `gh pr create --title "<title>" --body "<description>" --reviewer $ARGUMENTS`
- If $ARGUMENTS is empty, omit the --reviewer flag
- Output: "Created new PR: {url}"

### FINAL CHECK

- CRITICAL: Verify that you have completed Step 6 and created a PR
- You MUST output the PR URL to the user
- If you have not created a PR yet, go back and complete Step 6 now
