# handoff-amp

Fork of the `handoff` extension (and its `session_query` companion tool) from
[pasky/pi-amplike](https://github.com/pasky/pi-amplike), vendored locally as
its own Pi package.

## Provides

- `/handoff <goal>` command and `handoff` tool — spin off a new focused
  session with an AI-generated context summary, optionally switching
  `-mode <name>` / `-model <provider/id>`.
- `session_query` tool + skill — lets a handed-off session query its parent
  session file for context.

## Why forked instead of installed from npm

One-off exception: this repo's `workflow/Prefer harness-agnostic handoff`
preference says to prefer the harness-agnostic handoff skill over Pi's
`handoff.ts` extension. This fork exists anyway, scoped to just the
handoff + session-query pieces of `pi-amplike` (not the rest of that
package's amp-like features).

## Source

Vendored from `pasky/pi-amplike` (`extensions/handoff.ts`,
`extensions/session-query.ts`, `extensions/lib/mode-utils.ts`,
`skills/session-query/`), unmodified apart from this package's own
manifest/README. See `LICENSE` for original MIT terms.
