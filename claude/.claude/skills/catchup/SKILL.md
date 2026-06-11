---
name: catchup
description: Refresh context on the current repo — uncommitted changes, recent commits, open PRs (with full context for the current branch's PR), worktrees, stashes, plans, and handoff notes.
argument-hint: "[file-path]"
model: sonnet
allowed-tools: Bash(git status:*), Bash(git diff:*), Bash(git log:*), Bash(git worktree list:*), Bash(git stash list:*), Bash(git branch:*), Bash(gh pr list:*), Bash(gh pr view:*), Bash(gh pr checks:*), Bash(ls:*), Bash(test:*)
---

Re-orient on the current state of the repo and any outstanding work.

## Instructions

Gather the following in parallel where possible. Skip any that don't apply.

1. **Git status**: `git status` — uncommitted/staged files, current branch.
2. **Uncommitted diff**: `git diff` (and `git diff --staged` if anything is staged).
3. **Recent commits**: `git log --oneline -10`.
4. **Current branch's PR**: determine the current branch with `git branch --show-current`. Then try `gh pr view --json number,title,state,url,body,headRefName,baseRefName,isDraft,mergeable,reviewDecision,statusCheckRollup,comments`. If a PR exists:
   - Summarize title, state, draft status, base branch, mergeable status, review decision.
   - Summarize the PR body (intent of the change).
   - List review comments and their resolution status.
   - Surface failing/pending CI checks via the `statusCheckRollup` field (or `gh pr checks` if more detail is needed).
   If no PR is associated, skip silently.
5. **Other open PRs**: `gh pr list --author=@me` — just titles + CI status, for context beyond the current branch.
6. **Worktrees**: `git worktree list`.
7. **Stashes**: `git stash list`.
8. **Plans**: if `plans/` or `.plans/` exists at the repo root, list files and briefly summarize current plan(s).
9. **Handoff notes**: if `HANDOFF.md` (or similar) exists, read and summarize.

$ARGUMENTS

## Output

Present a concise summary organized by section. Lead with what needs attention:

- Uncommitted changes that should be committed or stashed
- Current-branch PR: unresolved review comments, failing CI, merge conflicts
- Other PRs with failing CI
- Stale worktrees
- Plan stages next up for implementation

Keep it brief and actionable — the user is re-entering this work and needs the shortest path back to productive.
