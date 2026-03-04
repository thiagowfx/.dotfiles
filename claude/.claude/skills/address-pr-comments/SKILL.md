---
name: address-pr-comments
description: Fetch PR review comments and address them
---

# Address PR Comments

Fetch unresolved review comments on the current branch's PR and address them.

## Instructions

### Step 1: Identify the PR

- Run `gh pr view --json url,number,headRefName` to find the PR for the current branch
- If no PR exists, stop and tell the user

### Step 2: Fetch Review Comments

- Fetch all review comments:

  ```bash
  gh api repos/{owner}/{repo}/pulls/{number}/comments --paginate
  ```

- Also fetch top-level review bodies:

  ```bash
  gh api repos/{owner}/{repo}/pulls/{number}/reviews --paginate
  ```

- Filter to unresolved comments (not outdated, not resolved). Ignore bot comments.
- Group comments by file and thread (use `in_reply_to_id` to reconstruct threads)

### Step 3: Summarize Comments

Present a numbered summary of all unresolved comments:

- Who left the comment
- Which file and line it applies to
- What the comment is asking for
- Quote the comment text briefly

Ask the user which comments to address (default: all).

### Step 4: Address Each Comment

For each selected comment:

1. Read the relevant file and surrounding context
2. Understand what the reviewer is asking for
3. Make the requested change
4. If a comment is unclear or you disagree with it, flag it to the user instead of guessing

### Step 5: Commit and Push

- Stage only the files that were modified to address comments
- Create a single commit with a message like: `address PR review comments`
  - In the commit body, briefly list which comments were addressed
- Push to the current branch

### Step 6: Report

Summarize what was done:

- Which comments were addressed and how
- Which comments were skipped and why (if any)
- Link to the PR

## Notes

- If a comment requires a design decision or is ambiguous, ask the user rather than guessing
- If a comment is already addressed by existing code, note that and move on
- Keep changes minimal and focused on what reviewers asked for
- Do not refactor or make unrelated changes while addressing comments
