# Pi configuration

Personal [Pi](https://github.com/badlogic/pi-mono) agent configuration, managed as dotfiles.

## Highlights

- Catppuccin Mocha theme
- Default model: Anthropic Claude Opus
- High thinking level, visible thinking blocks, compaction, retries, and caching
- Anthropic Claude models only: Haiku, Sonnet, Opus, Fable
- Telemetry disabled
- User-only session-tree filter

## Customizations

### Extensions

- **Session ID** — full session UUID in footer for quick reference.
- **Powerline status** — compact footer inspired by
  [`pi-powerline-footer`](https://pi.dev/packages/pi-powerline-footer?name=status), for model, thinking level,
  directory, branch, context, cache, session cost, and existing extension statuses; no editor, welcome, queue,
  or shell extras.
- **MCP startup list** — shows enabled MCP server names below loaded startup resources.
- **Side chat** — `/btw [question]` opens a context-aware, read-only side conversation without growing main history.
- **Prompt stash** — queue drafts with `Ctrl+s`, restore with `Ctrl+Shift+s`, or manage via `/stash`.
- **Double paste expansion** — large pastes collapse; paste same clipboard text again to expand it.
- **Command aliases** — `/clear` → `/new`; `/rename` → `/name`.
- **Dangerous command guard** — parses agent-issued shell with Tree-sitter, then blocks known destructive
  operations such as Terraform apply/destroy, hard resets, unsafe cleans, protected-branch force-pushes,
  and hook bypasses; dynamic command names bypass inspection; installs pinned parser
  dependencies automatically on first load.
- **Ready notifications** — terminal bell and macOS chime when agent finishes; no desktop notifications.
- **cmux integration** — reports Pi lifecycle and tool activity for idle detection, notifications, Feed telemetry,
  and session restore.
- **Automatic session names** — generates a concise name from the first prompt using the cheapest available model.
- **Title-bar spinner** — shows agent activity, session name, and working directory in terminal title.
- **Atuin history** — records agent-issued bash commands in Atuin under author `pi`; needs
  `atuin hook install pi`.
- **GitHub PR link** — statusline shows the session pull request as a clickable `PR #123`, including
  an active `.worktrees` checkout; one `gh pr view` call on session start and after each turn; needs authenticated `gh`.

### Prompt templates

- **`/commit`** — commits only files changed in current turn. Does not push.

### Interface

- **Theme:** Catppuccin Mocha dark palette with markdown, syntax, diff, tool, and thinking-level colors.
- **Model cycling (backward):** `Shift+Ctrl+P` or `Ctrl+N`; scoped models stay grouped by provider and
  ordered from cheaper/lighter to costlier/stronger.
- **Model/session save or sort:** `Alt+S`.
- **Fullscreen TUI:** transcript scrolls in viewport; editor and powerline status stay docked at bottom.

## Packages and extensions

### Git packages

- [`thiagowfx/skills`](https://github.com/thiagowfx/skills)
  - PR review-comment handling
  - Blog drafting
  - Repository catch-up and handoffs
  - GitHub Actions failure analysis
  - Design grilling, TDD, APKBUILD scaffolding
  - PR shipping and CI-pass loops

### npm packages

- `@tmustier/pi-session-recap` — focus-aware while-you-were-away recaps using a cheap same-provider model automatically.
- `@nerisma/pi-auto-title` — generates concise session names from first prompts using a cheap model.
- `@ryan_nookpi/pi-extension-memory-layer` — curated global/project memory with compact index injection and on-demand recall.
- `@zeldrisho/pi-web-fetch` — keyless, bounded public webpage fetching through sole `web_fetch` tool.
- `pi-tool-display` — OpenCode-style compact tool rendering and richer edit diffs.
- `pi-mcp-adapter` — lazy MCP server integration through one context-efficient proxy tool; persistent footer status disabled.
- `pi-team` — assembles multiple AI agents for parallel task analysis and synthesis.
- `@juicesharp/rpiv-ask-user-question` — lets model ask structured questions through terminal dialogs.
- `@juicesharp/rpiv-todo` — renders model todo lists as a live overlay that survives reloads and compaction.
- `@sreetej510/pi-usage` — reports provider usage and rate-limit budgets through `/usage` and a live statusline widget.

### Local packages

- `web-search` — keyless web discovery via Jina Reader and DuckDuckGo; runs with `python3`; adapted from
  [`pasky/pi-amplike`](https://github.com/pasky/pi-amplike) under MIT license.

## Layout

- `agent/AGENTS.md` — global instruction source of truth; `~/.claude/CLAUDE.md` links to it.
- `agent/settings.json` — Pi settings and packages.
- `agent/keybindings.json` — keybindings.
- `agent/extensions/` — custom TypeScript extensions.
- `agent/local/` — local Pi packages.
- `agent/themes/` — custom themes.
- `agent/check-pinned-npm-packages.py` — prek hook; rejects unpinned npm package specs.
