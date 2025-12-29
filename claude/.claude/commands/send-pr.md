---
allowed_tools: ["Bash(cat:*)", "Bash(gh api user:*)", "Bash(gh pr create:*)", "Bash(gh pr edit:*)", "Bash(gh pr view:*)", "Bash(git add:*)", "Bash(git checkout:*)", "Bash(git commit:*)", "Bash(git diff:*)", "Bash(git log:*)", "Bash(git push:*)", "Bash(git rev-parse:*)", "Bash(git status:*)", "Bash(test:*)", "SlashCommand"]
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

### Step 1: Handle Uncommitted Changes

- Check the git status output above for uncommitted changes
- If there are uncommitted changes (modified, added, or untracked files):
  - Use the SlashCommand tool to invoke /commit
  - Wait for the commit to complete before proceeding
  - If the commit fails, stop and report the error
  - CRITICAL: After the commit completes, you MUST continue with Steps 2-6 below to create the PR

### Step 2: Verify Prerequisites

- Check that there are commits to push (from git log output above)
- If no commits exist, stop and inform the user: "No commits to push. Create commits first."
- REMINDER: You are in the process of creating a PR. Continue with the remaining steps.

### Step 3: Handle Branch Creation

- If current branch is 'main' or 'master', create a new branch first:
  - Generate a descriptive branch name based on the changes (from git diff above)
  - Prefix it with the GitHub username and slash (e.g., "username/feature-name")
  - Use `git checkout -b <branch-name>` to create and switch to it

### Step 4: Push the Branch

- Push with upstream tracking: `git push -u origin <branch-name>`
- If push fails, report the error and stop

### Step 5: Check for PR Template

- Check if `.github/PULL_REQUEST_TEMPLATE.md` exists at the git repository root
- If it exists, read it with `cat` to use as the PR description template

### Step 6: Create or Update PR

- Check if a PR already exists for this branch: `gh pr view --json url 2>/dev/null`
- If PR exists:
  - Summarize the changes from the git diff above
  - Update the PR description: `gh pr edit --body "<updated-description>"`
  - Add reviewers if $ARGUMENTS provided: `gh pr edit --add-reviewer $ARGUMENTS`
  - Output: "Updated existing PR: {url}"
- If no PR exists:
  - Analyze all commits and changes to create a concise PR description
  - If template exists, follow its structure
  - Create PR with reviewers: `gh pr create --title "<title>" --body "<description>" --reviewer $ARGUMENTS`
  - If $ARGUMENTS is empty, omit the --reviewer flag
  - Output: "Created new PR: {url}"

### FINAL CHECK

- CRITICAL: Verify that you have completed Step 6 and either created or updated a PR
- You MUST output the PR URL to the user
- If you have not created/updated a PR yet, go back and complete Step 6 now
