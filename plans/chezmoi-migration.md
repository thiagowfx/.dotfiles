# Evaluating chezmoi as a replacement for Stow

## Context

The repo uses GNU Stow (symlinks) + Justfile (orchestration + platform gating) + git submodules
(vendoring) + prek (linting). [chezmoi][chezmoi] is the most commonly suggested alternative: it
manages a *source state* that is rendered into `$HOME`, with Go templates for per-machine
variance, declarative file modes, and built-in secret retrieval.

This document works out what adopting it would concretely involve, and concludes **not to**.
That conclusion is specific to this repo, not a judgement about chezmoi. The short version:

- The set of files that genuinely vary per machine is **~5 of 126 stowed files (4%)**.
  Everything else is already handled by tool-native includes and `command -v` guards.
- The lint stack keys on real file paths — and `.ls-lint.yml` lints *filenames*. chezmoi's
  source-state naming collides with both.
- Templating a file removes it from schema validation, because the source state is no longer a
  valid config file.
- The public/corp two-repo split is a first-class Stow idiom and an awkward one for chezmoi.

The genuine wins are real but narrow, and the best three have cheap Stow-side equivalents. They're
listed in [What to steal instead](#what-to-steal-instead).

## What the migration concretely looks like

### The 45 package directories cease to exist

This is the part that surprises people. chezmoi has **no grouping layer**. Its source state is a
single tree mirroring the destination, one source entry per target entry. There is no equivalent
of a Stow package.

`.chezmoiroot` does not save them. It is a file at the repo root containing a relative path;
chezmoi then treats that subdirectory as the source root. It buys exactly one thing — keeping
`Justfile`, `Brewfile`, `vendor/`, `plans/`, `.github/` out of the target state — and it is
necessary, but it does not preserve packages.

Concretely: **120 of the 126 stowed files** have at least one dot-prefixed path component and so
get renamed. `zsh/.zshrc` becomes `home/dot_zshrc`. `pi/.pi/agent/extensions/atuin.ts` becomes
`home/dot_pi/agent/extensions/atuin.ts`. Files currently in separate, independently-gated packages
that happen to target the same directory become siblings — `zsh/.zshrc.d/plugins.zsh` and
`pacman/.zshrc.d/pacman_command_not_found.zsh` both land in `home/dot_zshrc.d/`, so gating has to
become per-file.

Attribute prefixes are applied in a fixed order, and the `.tmpl` suffix comes last:

```text
encrypted_ → private_ → readonly_ → empty_ → executable_ → symlink_ → dot_ → name → .tmpl
```

Note `empty_`: chezmoi **deletes** managed files whose rendered content is empty unless they carry
that prefix.

Sketch of the result:

```text
.dotfiles/
├── .chezmoiroot                 # contains: home
├── Justfile  Brewfile  vendor/  plans/  .github/     # untouched by chezmoi
└── home/                                             # source root
    ├── .chezmoidata.toml        # the package→binary table
    ├── .chezmoiignore           # templated platform gating
    ├── .chezmoiexternal.toml    # replaces the 5 submodules
    ├── .chezmoitemplates/       # shared bodies (i3/sway, AGENTS.md/CLAUDE.md)
    ├── .chezmoiscripts/         # run_once_ / run_onchange_ bootstrap
    ├── dot_zshrc.d/plugins.zsh
    ├── dot_gitconfig.tmpl
    ├── private_dot_ssh/private_config.tmpl
    ├── dot_pi/agent/…           # 31 files
    └── Library/Application Support/Code/User/modify_settings.json
```

### Platform gating: `package_binaries` → templated `.chezmoiignore`

`.chezmoiignore` is always interpreted as a template (no `.tmpl` suffix), matches **target** paths,
and is read only from the source root. The current bash associative array ports to data plus a
loop:

```toml
# home/.chezmoidata.toml
[packages.iterm2]
binary = "/Applications/iTerm.app/Contents/MacOS/iTerm2"
paths  = ["Library/Application Support/iTerm2"]

[packages.pacman]
binary = "pacman"
paths  = [".zshrc.d/pacman_command_not_found.zsh"]   # a file, not a directory
```

```gotemplate
{{- range $name, $pkg := .packages }}
{{-   $found := false }}
{{-   if hasPrefix "/" $pkg.binary }}
{{-     if stat $pkg.binary }}{{ $found = true }}{{ end }}
{{-   else if lookPath $pkg.binary }}{{ $found = true }}{{ end }}
{{-   if not $found }}{{ range $pkg.paths }}{{ . }}
{{ end }}{{ end }}
{{- end }}
```

This is genuinely a bit better than the Justfile loop: it re-evaluates on every `chezmoi apply`
rather than only at `just install`, and gating granularity drops from package to path glob. Two
caveats: `.chezmoiignore` does **not remove** already-applied files (uninstall Logseq and
`~/.logseq/` lingers until `chezmoi forget`), and nothing checks that a newly added file was
assigned to a package.

### Submodules and `sync-upstream` → `.chezmoiexternal.toml`

Externals materialize into the **destination**, never the source repo — so this is a deployment
mechanism, not a vendoring one. That splits `vendor/` in two: things consumed by `$HOME` can become
externals; the four `vendor/schemas/*.json` consumed by prek at lint time **cannot**, and stay a
`just` curl loop.

```toml
[".zsh/zsh-autosuggestions"]
    type = "git-repo"
    url = "https://github.com/zsh-users/zsh-autosuggestions.git"
    refreshPeriod = "0"          # explicit refresh only, matching this repo's pinning habits
    [".zsh/zsh-autosuggestions".clone]
        args = ["--depth=1"]

[".vim/autoload/plug.vim"]
    type = "file"
    url = "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"

[".config/ghostty/shaders/cursor_tail.glsl"]
    type = "archive-file"
    url  = "https://github.com/sahaj-b/ghostty-cursor-shaders/archive/refs/heads/main.tar.gz"
    path = "ghostty-cursor-shaders-main/cursor_tail.glsl"
```

Five submodules to zero, `.gitmodules` deleted, `--recurse` dropped from the clone. But be honest
about what this is: a submodule gitlink is a reviewable SHA and a rollback point, and an external
is not. `just update-git` already runs `git submodule update --remote`, so both float — it's a
like-for-like swap, not a supply-chain improvement. `checksum.sha256` on `file`/`archive-file`
externals is available and would be an improvement over both.

### The 10 tracked symlinks

Six are clean or improve: `vim/.vimrc`, `nvim/.nvim.init.lua`, `profile/.aliases`, and
`vim/.vim/after/ftplugin/hgcommit.vim` become `symlink_` files whose *contents* are the target
path; the two pointing into `vendor/` (`zsh/.zshrc`, `gitui/…/key_bindings.ron`) disappear entirely
because externals write those paths directly.

**Two cannot be expressed safely.** `sway/config → i3/config` and `claude/CLAUDE.md → pi/AGENTS.md`
are cross-package. Under Stow the final hop lands *inside the repo*, which exists whether or not
the other package was stowed — so the link resolves on a Wayland box with no i3. A `symlink_` file
produces `~/.config/sway/config → ~/.config/i3/config`, and `~/.config/i3/config` may have been
pruned by `.chezmoiignore`. Silent dangling link.

The idiom is shared template bodies instead, which is arguably better since each file becomes
self-sufficient and can diverge:

```gotemplate
{{ template "i3-config" . }}
{{ if lookPath "swayidle" }}exec swayidle -w timeout 300 'swaylock -f'{{ end }}
```

Cost: one file becomes three (body + two stubs).

## The templating wins

These are real. Five files, six values:

```gotemplate
# home/private_dot_ssh/private_config.tmpl
IdentityAgent "{{ if eq .chezmoi.os "darwin" -}}
~/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock
{{- else -}}~/.1password/agent.sock{{- end }}"
```

```gotemplate
# home/dot_logseq/preferences.json.tmpl — the file had two different usernames baked in
"url": "file://{{ .chezmoi.homeDir }}/.logseq/plugins/logseq-bonofix-theme/custom.css"
```

Plus `.name`/`.email` declared once in `.chezmoi.toml.tmpl` and consumed by `dot_gitconfig.tmpl`,
`dot_hgrc.tmpl`, and `dot_config/jj/config.toml.tmpl`; a probe for the Homebrew prefix; and an
OS-guarded `afplay` in `notify.sh`.

Since 1Password CLI is already installed, chezmoi could also read secrets at apply time —
`{{ onepasswordRead "op://Personal/…" }}` — which Stow structurally cannot do, since it only
deploys bytes that already exist in the repo.

**But note what those six values have in common**: every one of them was a plain bug, and all six
are now fixed without a templating engine (see [The bugs](#the-bugs)). Templating would have
prevented them; it was not required to fix them.

## What it costs

### The lint stack is keyed on real paths

`.pre-commit-config.yaml` contains roughly 30 hard-coded path patterns. The `pretty-format-json`
exclude alone is an 11-alternative literal-path regex. Every one changes, plus:

| Thing | Why it breaks |
| --- | --- |
| `.talismanrc` | keys on `filename:` + `checksum:`; all three filenames move |
| `.ls-lint.yml` `ignore:` | six literal paths, four of which stop existing |
| local `ghostty` hook | `--config-file=ghostty/.config/ghostty/config` is in the `entry:` |
| local pi hook | path in `entry:` **and** `SETTINGS = Path(...)` inside the script |
| `check-symlinks` | 8 of 10 symlinks become plain text files; nothing validates their contents |
| `check-executables-have-shebangs` | modes move into filenames, so it finds no executables at all |

The `meta` hooks (`check-hooks-apply`, `check-useless-excludes`) turn this from archaeology into a
checklist the tooling generates — but it's still a day of grinding.

### Templating silently removes files from validation

This is the sharper cost. Hook file-selection is extension-based:

- `shellcheck` selects on `.sh`. A `run_onchange_install_packages.sh.tmpl` has extension `.tmpl`,
  is not tagged as shell, and **stops being checked with no error**. Forcing it doesn't help —
  `{{ if eq .chezmoi.os "darwin" }}` is not valid shell.
- `pretty-format-json` and the `check-jsonschema` pi-theme hook likewise skip any `.json.tmpl`.
  A templated JSON file is not JSON, so you don't exclude it, you lose validation of it.

CI here validates *rendered* configs. chezmoi's source state is not a rendered config. The
mitigation is a render-then-lint hook plus a `macos-latest` CI leg (since
`chezmoi execute-template` fills `.chezmoi.os` from the runner, so a Linux runner only ever
exercises the Linux branch) — real, but real added cost.

### `.ls-lint.yml` collides with chezmoi's naming vocabulary

The repo lints filenames: `.sh/.bash/.zsh: snake_case`, `.json/.toml/.yaml/.yml: kebab-case`.

- `modify_settings.json` → basename `modify_settings` → **fails kebab-case**. Same for any
  `private_*.json`.
- `run_once_after_configure-macos.sh` mixes `_` and `-` → **fails snake_case**. (chezmoi's own docs
  use hyphens in script names.)
- `.sh.tmpl` matches no rule, so templated scripts silently lose filename linting entirely.

Fixable by relaxing `.json` to a regex rule — i.e. by weakening the check.

### The live-edit loop

Today `vim ~/.zshrc` *is* editing `~/.dotfiles/zsh/.zshrc`, and `git diff` shows it. Under chezmoi's
default copy mode you need `chezmoi edit --apply`, or `chezmoi re-add` after the fact, in both
directions. This matters more here than in most repos: `pi/.pi/agent/settings.json` is rewritten by
pi itself and was touched 22 times in a recent 12-day window. Under Stow those writes land in the
repo for free.

`mode = "symlink"` recovers this — but only for non-templated, attribute-free files. So it restores
Stow's behavior for the ~121 files that never needed chezmoi, and leaves copy semantics on exactly
the handful that motivated the migration.

### `exact_` is a live foot-gun

One `exact_` on `dot_profile.d/`, `dot_zshrc.d/`, `private_dot_ssh/config.d/`, or
`dot_claude/skills/` makes `chezmoi apply` **delete** the corp drop-ins living there, on every run.
The default is the tolerant behavior we want, and nothing warns you. It would have to be a
documented prohibition.

## Why the corp split argues against it

`~/.dotfiles` and `~/.dotfiles_corp` are two independent Stow sources targeting one `$HOME`, each
separately `git status`-able and unstowable. `packages_no_folding := "claude espanso"` exists
precisely so `~/.claude/skills` stays a real directory both repos can write into. Every seam is
tool-native: `[include] path = .gitconfig_corp`, `%include .hgrc_corp`,
`if "[ -f ~/.tmux.conf_corp ]"`, `Include config.d/*`, and the `.profile.d`/`.zshrc.d`/`.bashrc.d`
drop-in dirs.

chezmoi is architecturally single-source. `--source` replaces the source dir, it does not layer;
there is no overlay stack. Running a second instance for corp means a second config file, a second
source dir, and a separate `--persistent-state` database, and each instance's `chezmoi status`
treats the other's files as noise.

Note also that tool-native seams keep working *without the manager*: `Include config.d/*` works on
a box where the repo was never cloned. A `.tmpl` is inert without the chezmoi binary and its
source dir.

The one clean win in this area: `--no-folding` becomes unnecessary, because chezmoi always creates
real directories.

## Verdict

**Don't migrate.** The costs above are concentrated in this repo's two distinguishing features —
an unusually path-coupled lint stack and a two-repo public/corp split — and the benefit is
concentrated in 4% of the files.

There is also precedent worth reading: [`nix-home-manager-migration.md`](nix-home-manager-migration.md)
is a careful 300-line plan that was never executed. Its closing section — "use Nix only for package
management… ~60% of the benefit for ~20% of the effort" — is the right instinct here too.

### What would flip it

1. **Ephemeral or semi-trusted hosts.** `sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply
   thiagowfx` needs only `curl`, against `git clone --recurse && just install` which needs git,
   perl+stow, just, and **bash 4+** — which stock macOS `/bin/bash` 3.2 is not. The most plausible
   trigger.
2. **Windows, or anywhere symlinks are second-class.** Stow is a non-starter there.
3. **~5+ heterogeneous machines.** Two machines with a clean corp split is the sweet spot for
   tool-native includes.
4. **Secrets belonging in the public repo.** chezmoi's age/1Password integration wins outright;
   Stow has no answer. Today they correctly live in `~/.dotfiles_corp`.
5. **Merging corp into this repo.** That removes the strongest structural argument above.
6. **Templated values crossing ~15–20 across ~10 files.** Tool-native seams scale in
   mechanisms-to-know; templates scale in one mechanism. There is a crossover.

## What to steal instead

Ranked by value ÷ effort. **None of these are implemented yet** — they are follow-ups, not part of
the commit that added this document.

1. **A `just doctor` drift check.** The one real capability gap. `chezmoi status`/`verify` exist
   because the copy model creates drift; Stow's drift surface is much smaller but not zero — an app
   that rewrites atomically (write-temp + rename, which VS Code and most Electron apps do) silently
   *replaces* the symlink with a regular file, and `chkstow` only finds dangling links, not
   replaced ones. Three layers, ~25 lines: `stow -n -v` (the closest analogue to `chezmoi diff`),
   `chkstow -t`/`-a`, and a loop asserting every tracked file resolves back into the repo.
2. **Pin and checksum `just sync-upstream`.** It curls six files from **branch HEADs** with no
   integrity check, in a repo that freezes every prek hook to a SHA, pins every pi npm package with
   a custom enforcement hook, and lints its Actions with zizmor. One of those files,
   `vendor/grml-etc-core/etc/zsh/zshrc`, is symlinked to `~/.zshrc` and executes on every shell
   start. Change the map from `path → url` to `path → url + sha256` and verify after download.
   This is the actual good idea inside `.chezmoiexternal.toml`. While there:
   `vendor/ghostty-cursor-shaders` is a whole submodule for one `.glsl` file and is a natural
   `sync-upstream` entry.
3. **`run_onchange_`-style input hashing for `bootstrap`.** `just install` re-runs a full
   `brew bundle` and a sudo-prompting `defaults write` block every time. Both are idempotent, so
   this buys speed and one fewer sudo prompt, not correctness — hash the inputs into
   `${XDG_CACHE_HOME:-$HOME/.cache}/dotfiles/<recipe>.sha256` and skip when unchanged.
4. **`chmod 700 ~/.ssh` in the `stow` recipe.** The entire declarative-permissions gap for this
   repo, in two lines. Exec bits already work — git tracks mode 0755 and the symlink resolves to
   the repo file — and `~/.ssh/config` at 0644 is fine, since ssh only rejects group/world-*writable*
   config and no private keys live here. The only real exposure is a fresh machine where Stow
   creates `~/.ssh` itself.

Deliberately **not** worth stealing: templating (4% of files, and it costs schema validation),
copy-mode apply, source-name attributes, and `.chezmoiignore` — the `package_binaries` probe array
is the better version of that idea, since it is capability-based and prints what it skipped.

## The bugs

Auditing for "what would templating fix?" surfaced seven defects that had nothing to do with the
dotfile manager. All are fixed on this branch.

| File | Bug |
| --- | --- |
| `claude/.claude/hooks/notify.sh` | `afplay` ran unguarded on Linux — the guard already existed in the sibling `pi/.pi/agent/extensions/notify.ts` |
| `profile/.profile.d/10_brew.sh` | hardcoded `/opt/homebrew`, though the file's own comment documented the Intel `/usr/local` case; no Linuxbrew fallback |
| `ssh/.ssh/config` | macOS-only 1Password socket hardcoded, Linux path present only as a comment; now two `Match exec` probes |
| `Justfile` (`unstow`) | omitted `packages_no_folding`, so `claude` and `espanso` stayed linked |
| `Justfile` (`swiftbar`) | added the missing inverse check - the loops warned about packages without a mapping but never the reverse. `swiftbar` is a known placeholder (the cask is installed, the package isn't written yet), so it warns rather than fails |
| `Justfile` (`espanso`) | gated on the `espanso` binary, which exists on Linux, but the package only ships the macOS `Library/…` path |
| `logseq/.logseq/preferences.json` | contained **two different usernames** (`/Users/thiago.perrotta` and `/Users/tperrotta`) — write-through captured each machine's state into a different theme key. Dropped the active-selection pointer only; the theme itself stays declared and version-pinned in `logseq/.logseq/config/plugins.edn` |

Seven bugs, zero of which needed chezmoi. That is the argument in miniature.

### Known gaps, not fixed

- `ssh/.ssh/config` has `Include config.d/*` at the end, after `Host *`. ssh_config takes the
  **first** obtained value, so nothing in `config.d/` — including corp drop-ins — can override a
  `Host *` directive. Possibly deliberate; moving it changes corp behavior.
- `jj/.config/jj/config.toml` has no corp include where git and hg both do. jj supports
  `~/.config/jj/conf.d/`; filling this needs a corp-side change too.
- `tmux/.tmux.conf` settles on `screen-256color` as a macOS/Linux compromise. The comments suggest
  this is deliberate; the real fix is installing `tmux-256color` terminfo, not a config change.

## Verification

If this is ever revisited, the migration is testable without committing to it:

```bash
chezmoi --source=home --destination="$(mktemp -d)" init --apply --no-tty
chezmoi --source=home --destination="$(mktemp -d)" verify
chezmoi --source=home execute-template < home/.chezmoiignore   # compare against `just stow` output
```

That last command is the cheapest way to sanity-check the gating port: its output should match the
"Stowing / Skipping" log the current `stow` recipe prints.

[chezmoi]: https://www.chezmoi.io/
