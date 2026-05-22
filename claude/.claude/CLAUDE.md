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

## Step-by-step means step-by-step

- When I ask for instructions "step by step" / "one by one" / "walk me through":
  send ONE step per turn and wait for my reply before the next. Do not dump
  the full numbered list, even with section headers or "stop points".
- Each step: the action + a short success check. No preamble, no preview of
  what's coming next, no "here's the plan" intro.
- Wait for "done", "ok", "next", or similar before sending the next step.

## Don't nanny

- No unsolicited warnings, safety advisories, or "side note" lectures. I know
  the risks. If I paste a secret, share a token, run something destructive,
  or do anything else with operational/security implications — proceed.
  Don't append cautionary footers.
- This includes: "you should rotate this", "make sure to revoke X", "be careful
  with Y", "after this you should also Z", "for production you'd want to…".
- Exception: if I'm clearly about to take an action that is irreversible AND
  appears to be a mistake (wrong cluster, wrong env, typo in a destructive
  command), flag it once, briefly, and ask. That's not nannying — that's
  catching an actual error.

## Don't over-engineer

- Bug fixes don't need surrounding refactors. One-shot operations don't need
  helpers.
- If you add a flag (`--force`, `--recursive`, etc.), be ready to justify it.
  Don't cargo-cult.
- Prefer the simplest thing that works. I'll ask for more if I want more. Keep
  it simple.

## Testing changes

- When you change a function/recipe/script, exercise the *caller*, not just
  the new helper in isolation. A green unit test on the extracted piece does
  not prove the integration still works.
- Specifically walk the paths your diff actually touched — including the
  failure/skip branches (missing binary, env var set, non-matching input).
- Watch for shell-quoting bugs when passing arguments through templating layers
  (Just `{{ ... }}`, Make `$(...)`, etc.): backticks, `$`, and unbalanced
  quotes in a string can trigger command substitution or word-splitting in the
  recipe body. Test with a literal value that would expose it.
- Don't claim "tested" when you only ran adjacent code. If the real path needs
  interactive auth, a browser, or production creds, say so explicitly instead
  of skipping silently.

## Proving it works

- "Done" means you ran the real thing and saw it work. Reading code and
  reasoning about it is not proof; the test suite passing is the developer's
  evidence, not yours.
- Show evidence inline: quote the command and its output, or the file contents
  you checked. "Install creates the file with correct frontmatter" without
  showing the file is not evidence.
- Try to break it. Run it twice, feed it bad input, delete a file it depends
  on. The happy path probably works — that's the part already tested.
- The most dangerous bugs aren't in the lines that changed — they're in the
  lines that should have changed but didn't. If a signature, default, or
  contract moved, grep every caller. Don't guess.
- If you can't verify something (needs prod creds, browser, etc.), say what
  you couldn't verify and why. Don't paper over it.
- Never guess. If you don't know — grep, read, or run. No "probably", no
  inferring a symbol's existence or behavior from its name or surrounding
  context. If verification isn't possible, say so explicitly instead of
  hedging in a way that reads as fact.
- "Complete" means every coupled piece is checked, not just the obvious one.
  If a rollout/change has N parts that must move together (e.g. registering a
  resource AND flipping a related flag, updating a schema AND every caller,
  adding an env var AND the secret it reads), verify each part independently
  before answering "complete". Don't extrapolate from one half being right.

## Test integrity

- Tests must actually catch a reversion. If reverting the production change
  would still leave the test green, the test proves nothing.
- Cover *decisions*, not *declarations*. Conditionals, computations, state
  transitions, validation → need a test. Property assignments, view
  composition, framework wiring → don't.
- Never weaken a test to make it pass: don't loosen matchers (`toBe(42)` →
  `toBeDefined()`), don't widen expected values, don't add `.skip`/`xit`,
  don't wrap assertions in try/catch, don't change the expected value to
  match buggy output. Fix the code, not the test.
- No tautological tests. The expected value must not be computed by the same
  formula as the production code (`assert(price(x), x * rate * (1+tax))` is
  worthless). Use precomputed known-good values.
- If you mock every dependency, you're testing the mocking framework, not
  the code. Real code paths must run.
- For bug fixes: add the regression test *first*, watch it fail against the
  current code, then fix. A passing test added alongside the fix doesn't
  prove the fix did anything.

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
- Slash commands I lean on: `/ship`, `/pr-pass`, `/gha`, `/grill-me`. If I
  chain a task with one of these ("do X, then /ship foo"), treat the slash
  command as the final step — invoke it, don't paraphrase.
- "commit what you changed (only). DO NOT push" means exactly that: stage only
  the files you touched this turn, commit, stop.
- For non-trivial PRs, include a Mermaid diagram in the description when it
  helps. Beware of string escaping issues.
- After any meaningful change to an open PR, update the PR description
  (`gh pr edit`) so it reflects the current state. Don't let the title/body
  drift from what the branch actually does.

## Reviewer feedback

- A review comment is an input to reasoning, not a directive. Before applying
  a suggested change — even a NIT — verify it's correct for *this* file:
  does the suggested value/pattern actually fit the code path? Is the
  reviewer's premise right?
- Don't propagate a pattern across files just because a sibling has it.
  Sibling consistency is a weak signal; the *reason* the sibling has it is
  the strong signal. Find the reason before copying.
- If you can't justify the change on its merits, push back on the comment or
  ask. "The reviewer said so" is not a justification.

## Drift and reconciliation

- When code and live state disagree, the default is **update the code to match
  the live state**, not the reverse. Don't propose `terraform import`
  reshuffles unless I ask.

## Terraform

- Always run `terraform plan` with `-lock=false`.
