# Global CLAUDE.md

Personal preferences that apply across every project. Project-specific facts live in each repo's own CLAUDE.md.

## Tone

- Terse. Action-oriented. No lectures, no preamble, no trailing summaries, no
  political correctness.
- Don't invent sections, headings, or boilerplate I didn't request. Match the
  shape of what I asked for.
- Number multi-part questions (1/2/3) so I can answer inline.
- When disambiguating, focus on one topic at a time.
- If I reply "yes", "go on", "a)", "do it", or similar — that's full
  authorization. Don't re-confirm.

## Keep moving

- If my reply is "hello?", "so?", "are you still there?" — you stalled. Don't
  wait for trivial confirmation; pick the reasonable default and continue.
- After a sandbox lift ("try again", "I lifted your sandbox") — retry the exact
  same command. Don't restart reasoning.
- Don't pause to summarize what you just did before doing the next step.

## Don't over-engineer

- Bug fixes don't need surrounding refactors. One-shot operations don't need
  helpers.
- If you add a flag (`--force`, `--recursive`, etc.), be ready to justify it.
  Don't cargo-cult.
- Prefer the simplest thing that works. I'll ask for more if I want more. Keep
  it simple.

## Destructive ops

- Never `rm`. Use `trash` (or the equivalent move-to-trash).
- For destructive ops on shared state (S3 objects, branches, DB rows): make a
  backup first, then delete only what matches, then confirm. Phrase:
  *backup → filter → delete*.
- Never force-push to `master`/`main`. Never skip hooks (`--no-verify`) unless I
  explicitly ask.

## Tooling defaults

- **Pre-commit**: prefer `prek` over `pre-commit`. Run
  `prek run --all-files`. When configuring hooks, prefer self-contained/pinned
  deps over relying on system binaries.
- **Polling**: never `sleep` in a loop. Use the `Monitor` tool with an `until`
  condition, or `run_in_background` and wait for the notification.
- **`gh pr checks`**: `--watch` and `--json` are mutually exclusive. Don't
  combine them.
- **Editor**: vim (preferred) and Zed. Don't suggest VSCode-specific workflows.

## Worktrees and PRs

- Default to worktrees for parallel work. Path convention:
  `~/<org>/<repo>/.worktrees/<topic>/`. If I say "work here please: `<path>`",
  `cd` there and proceed. Use the `wt` tool to manage worktrees.
- Branch prefix: `thiagowfx/<topic>`.
- Slash commands I lean on: `/commit`, `/commit-and-send-pr`, `/send-pr`,
  `/pr-pass`, `/gha`, `/grill-me`. If I chain a task with one of these ("do X,
  then /commit-and-send-pr foo"), treat the slash command as the final step —
  invoke it, don't paraphrase.
- "commit what you changed (only). DO NOT push" means exactly that: stage only
  the files you touched this turn, commit, stop.
- For non-trivial PRs, include a Mermaid diagram in the description when it
  helps. Beware of string escaping issues.
- After any meaningful change to an open PR, update the PR description
  (`gh pr edit`) so it reflects the current state. Don't let the title/body
  drift from what the branch actually does.

## Drift and reconciliation

- When code and live state disagree, the default is **update the code to match
  the live state**, not the reverse. Don't propose `terraform import`
  reshuffles unless I ask.
