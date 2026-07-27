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

### Extensions

- **Caveman mode** — auto-enabled terse responses; toggle with `/caveman on|off`.
- **Plan mode** — read-only exploration with `/plan` or `Ctrl+Alt+P`; extracts plans and tracks execution progress.
- **Side chat** — `/btw [question]` opens a context-aware, read-only side conversation without growing main history.
- **Prompt stash** — queue drafts with `Ctrl+s`, restore with `Ctrl+Shift+s`, or manage via `/stash`.
- **Command aliases** — `/clear` → `/new`; `/rename` → `/name`.
- **Dangerous command guard** — blocks agent-issued destructive commands: `rm -rf`, Terraform apply/destroy,
  hard resets, unsafe cleans, protected-branch force-pushes, and hook bypasses.
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

### npm packages

- `@tifan/pi-recap` — session recap generation.
- `@tintinweb/pi-subagents` — specialized subagents.
- `@tmustier/pi-raw-paste` — raw paste support.
- `@tmustier/pi-tab-status` — tab status widget.

### Local package

- `pi-memory-async` — persists exit-summary jobs and writes durable daily memory asynchronously on a later session.

## Layout

- `agent/AGENTS.md` — global instruction source of truth; `~/.claude/CLAUDE.md` links to it.
- `agent/settings.json` — Pi settings and packages.
- `agent/keybindings.json` — keybindings.
- `agent/extensions/` — custom TypeScript extensions.
- `agent/themes/` — custom themes.
