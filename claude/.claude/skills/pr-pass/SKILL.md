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

### Step 3: Poll CI and Act Eagerly

Poll CI checks repeatedly and start analyzing failures as soon as individual checks complete — don't wait for all checks to finish.

**Important:** `--watch` and `--json` are mutually exclusive in `gh pr checks`. Never combine them.

- Poll with:

  ```bash
  gh pr checks <number> --json name,state,bucket,link
  ```

- On each poll iteration:
  1. If any check has `"state": "FAILURE"` — immediately start analyzing and fixing it (Step 4)
  2. If all checks have completed (no `"state": "PENDING"`) and all passed — report success and stop
  3. If checks are still pending and none have failed yet — wait ~30 seconds and poll again

- This eager approach means you can start diagnosing and fixing the first failure while slower checks are still running.

### Step 4: Analyze Failures

- For each failed check, get the logs:

  ```bash
  gh run view <run-id> --log-failed
  ```

- Identify the root cause of each failure (test failures, lint errors, build errors, type errors, etc.)
- Distinguish between flaky tests and real failures by checking if the same test passed in recent runs

### Step 5: Fix

- Fix the identified issues in the code
- Run relevant local checks (tests, lint, build) to verify the fix before pushing
- Commit the fix with a clear message describing what was fixed

### Step 6: Loop

- Go back to Step 1
- Maximum 5 iterations to avoid infinite loops
- If still failing after 5 attempts, report the remaining failures and stop

## Notes

- If a failure looks like a flaky test (passes locally, intermittent history), re-push without changes to retry CI
- If a failure is in infrastructure (CI config, permissions, external service), report it and stop rather than looping
- Always run local verification before pushing to avoid wasting CI cycles
