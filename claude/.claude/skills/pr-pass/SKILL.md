---
name: pr-pass
description: Push, wait for CI, fix failures, and loop until all checks pass
---

# PR Pass — Push and Fix Until Green

Push your code and loop: wait for CI, analyze failures, fix them, and re-push until all checks pass.

## Instructions

### Step 1: Push

- Run `git push` to push the current branch
- If no upstream is set, run `git push -u origin $(git branch --show-current)`

### Step 2: Find the PR

- Run `gh pr view --json url,number,headRefName` to find the PR for this branch
- If no PR exists, stop and tell the user to create one first (e.g., `/send-pr`)

### Step 3: Wait for CI

- Poll CI status every 30 seconds using:

  ```bash
  gh pr checks --json name,state,conclusion --watch
  ```

- Once all checks have completed, proceed to Step 4

### Step 4: Evaluate Results

- If all checks passed: report success and stop
- If any checks failed: continue to Step 5

### Step 5: Analyze Failures

- For each failed check, get the logs:

  ```bash
  gh run view <run-id> --log-failed
  ```

- Identify the root cause of each failure (test failures, lint errors, build errors, type errors, etc.)
- Distinguish between flaky tests and real failures by checking if the same test passed in recent runs

### Step 6: Fix

- Fix the identified issues in the code
- Run relevant local checks (tests, lint, build) to verify the fix before pushing
- Commit the fix with a clear message describing what was fixed

### Step 7: Loop

- Go back to Step 1
- Maximum 5 iterations to avoid infinite loops
- If still failing after 5 attempts, report the remaining failures and stop

## Notes

- If a failure looks like a flaky test (passes locally, intermittent history), re-push without changes to retry CI
- If a failure is in infrastructure (CI config, permissions, external service), report it and stop rather than looping
- Always run local verification before pushing to avoid wasting CI cycles
