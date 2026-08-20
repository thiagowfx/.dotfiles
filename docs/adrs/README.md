# Architecture Decision Records

Significant decisions for this dotfiles repo — how configs are stored, deployed, and
kept in sync across machines.

## ADRs

| Number | Title | Status | Date |
| ------ | ----- | ------ | ---- |
| [0001](0001-nix-home-manager-migration.md) | Nix and Home Manager | Abandoned | 2026-02-23 |
| [0002](0002-chezmoi-migration.md) | chezmoi as a Stow Replacement | Rejected | 2026-08-15 |

## Adding New ADRs

1. Create a new file: `NNNN-short-title.md` (increment the number).
2. Use this template:

```markdown
# ADR-NNNN: Title

## Status

Proposed | Accepted | Partially Accepted | Rejected | Abandoned | Deprecated | Superseded

## Date

YYYY-MM-DD

## Context

What is the issue that we're seeing that is motivating this decision?

## Decision

What is the change that we're proposing and/or doing?
Include its rationale.
Include alternatives considered where applicable.

## Consequences

What becomes easier or more difficult to do because of this change?
```
