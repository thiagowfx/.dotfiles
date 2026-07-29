# Pi configuration

Personal [Pi](https://github.com/badlogic/pi-mono) agent configuration, managed as dotfiles.

## Highlights

- Catppuccin Mocha theme
- Default model: OpenAI Codex GPT Terra or Anthropic Claude Sonnet equivalent
- High thinking level, visible thinking blocks, compaction, retries, and caching
- Claude and OpenAI Codex models
- Telemetry disabled
- User-only session-tree filter

## Customizations

### Workflows

- **External Superpowers artifacts** — `piss` stores specs and plans outside source repositories in active
  Obsidian vault under `superpowers/<github-owner>/<repository>/{specs,plans}`; dedicated qmd collection
  `superpowers` provides cross-repository search, and handoffs include portable paths plus one-click
  Obsidian links.

### Extensions

- **Caveman mode** — auto-enabled terse responses; toggle with `/caveman on|off`.
- **Context moon** — accent-colored moon-phase gauge tracks context-window usage in footer.
- **Plan mode** — read-only exploration with `/plan` or `Ctrl+Alt+P`; extracts plans and tracks execution progress.
- **Side chat** — `/btw [question]` opens a context-aware, read-only side conversation without growing main history.
- **Prompt stash** — queue drafts with `Ctrl+s`, restore with `Ctrl+Shift+s`, or manage via `/stash`.
- **Double paste expansion** — large pastes collapse; paste same clipboard text again to expand it.
- **Command aliases** — `/clear` → `/new`; `/rename` → `/name`.
- **Dangerous command guard** — parses agent-issued shell with Tree-sitter, then blocks destructive
  operations such as `rm -rf`, Terraform apply/destroy, hard resets, unsafe cleans, protected-branch
  force-pushes, and hook bypasses; installs pinned parser dependencies automatically on first load.
- **Ready notifications** — native terminal/OS notification and macOS chime when agent finishes.
- **Title-bar spinner** — shows agent activity, session name, and working directory in terminal title.

### Interface

- **Theme:** Catppuccin Mocha dark palette with markdown, syntax, diff, tool, and thinking-level colors.
- **Model cycling:** `Shift+Ctrl+P` or `Ctrl+N`.
- **Model/session save or sort:** `Alt+S`.

## Packages and extensions

### Git packages

- [`thiagowfx/skills`](https://github.com/thiagowfx/skills)
  - PR review-comment handling
  - Blog drafting
  - Repository catch-up and handoffs
  - GitHub Actions failure analysis
  - Design grilling, TDD, APKBUILD scaffolding
  - PR shipping and CI-pass loops
- [`obra/superpowers@v6.2.0`](https://github.com/obra/superpowers/releases/tag/v6.2.0)
  - Disabled by default; `piss` loads upstream workflows plus local Obsidian artifact routing.

### npm packages

- `@narumitw/pi-goal` — persistent, verifiable autonomous goal completion with `/goal`.
- `@zeldrisho/pi-web-fetch` — keyless, bounded public webpage fetching through sole `web_fetch` tool.
- `@heyhuynhgiabuu/pi-diff` — Shiki-powered syntax-highlighted, word-level tool diffs.
- `@heyhuynhgiabuu/pi-pretty` — collapsed, syntax-highlighted tool output with FFF-backed `find` and `grep`.
- `@tifan/pi-recap` — session recap generation.
- `@tintinweb/pi-subagents` — specialized subagents.
- `@tmustier/pi-tab-status` — tab status widget.
- `pi-mcp-adapter` — lazy MCP server integration through one context-efficient proxy tool.

### Local package

- `pi-memory-async` — persists exit-summary jobs and writes durable daily memory asynchronously on a later session.

## Layout

- `agent/AGENTS.md` — global instruction source of truth; `~/.claude/CLAUDE.md` links to it.
- `.config/mcp/mcp.json` — shared MCP server configuration.
- `agent/settings.json` — Pi settings and packages.
- `agent/keybindings.json` — keybindings.
- `agent/extensions/` — custom TypeScript extensions.
- `agent/local/superpowers-artifacts.ts` — `piss`-only Obsidian artifact-routing overlay.
- `agent/themes/` — custom themes.
