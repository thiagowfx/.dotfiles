# ADR-0002: chezmoi as a Stow Replacement

## Status

Rejected

## Date

2026-08-15

## Context

The repo uses GNU Stow (symlinks), a Justfile (orchestration and platform gating), git
submodules (vendoring), and prek (linting). [chezmoi][chezmoi] is the alternative most
often suggested. It manages a *source state* that is rendered into `$HOME`, with Go
templates for per-machine variance, declarative file modes, and built-in secret
retrieval.

An audit measured what adopting it would cost here.

### The package directories cease to exist

chezmoi has no grouping layer. Its source state is a single tree that mirrors the
destination, one source entry per target entry. There is no equivalent of a Stow package,
so the 45 package directories disappear.

`.chezmoiroot` does not save them. It only moves the source root into a subdirectory,
which keeps `Justfile`, `Brewfile`, `vendor/`, and `.github/` out of the target state.
That is necessary but unrelated to grouping.

120 of the 126 stowed files have a dot-prefixed path component, so they get renamed:
`zsh/.zshrc` becomes `home/dot_zshrc`. Files from separately gated packages that target
the same directory become siblings. `zsh/.zshrc.d/plugins.zsh` and
`pacman/.zshrc.d/pacman_command_not_found.zsh` both land in `home/dot_zshrc.d/`, so
gating has to become per-file.

Source-name attributes are applied in a fixed order, with the template suffix last:

```text
encrypted_ → private_ → readonly_ → empty_ → executable_ → symlink_ → dot_ → name → .tmpl
```

`empty_` matters: chezmoi deletes managed files whose rendered content is empty unless
they carry that prefix.

### Platform gating and vendoring do port cleanly

The Justfile's `package_binaries` array becomes `.chezmoidata.toml` plus a loop in a
templated `.chezmoiignore`. That is slightly better than the current loop, because it
re-evaluates on every `chezmoi apply` instead of only at `just install`, and granularity
drops from package to path glob. Two caveats: `.chezmoiignore` does not remove
already-applied files, and nothing checks that a new file was assigned to a package.

The five submodules become `.chezmoiexternal.toml` entries. But externals materialize
into the destination, never into the source repo, so `vendor/schemas/*.json` (consumed by
prek at lint time) cannot be externals and stay a `just` curl loop. A submodule gitlink is
a reviewable SHA and a rollback point; an external is not. Since `just update-git` already
runs `git submodule update --remote`, both float. It is a like-for-like swap.

### Two tracked symlinks cannot be expressed safely

Of the 10 tracked symlinks, six become `symlink_` files and two disappear because
externals write those paths directly. The remaining two are cross-package:
`sway/config → i3/config` and `claude/CLAUDE.md → pi/AGENTS.md`. Under Stow the final hop
lands inside the repo, which exists whether or not the other package was stowed, so the
link resolves on a Wayland box with no i3. A `symlink_` file instead produces
`~/.config/sway/config → ~/.config/i3/config`, and that target may have been pruned by
`.chezmoiignore`. The result is a silent dangling link. The chezmoi idiom is a shared
template body plus two stubs, which is arguably better but turns one file into three.

### The templating win is 4% of the files

Five files and six values genuinely vary per machine: the 1Password agent socket path,
two hardcoded usernames in a Logseq preference file, the name and email shared by
gitconfig, hgrc, and the jj config, the Homebrew prefix, and an OS-guarded `afplay` call.
Everything else is already handled by tool-native includes and `command -v` guards.

Since the 1Password CLI is installed, chezmoi could also read secrets at apply time
through `onepasswordRead`, which Stow structurally cannot do.

But all six varying values were plain bugs, and all six are now fixed without a
templating engine (see [The bugs](#the-bugs)). Templating would have prevented them. It
was not required to fix them.

### The lint stack is keyed on real paths

`.pre-commit-config.yaml` holds roughly 30 hardcoded path patterns; the
`pretty-format-json` exclude alone is an 11-alternative literal-path regex. All of them
change, plus:

| Thing | Why it breaks |
| --- | --- |
| `.talismanrc` | keys on `filename:` and `checksum:`; all three filenames move |
| `.ls-lint.yml` `ignore:` | six literal paths, four of which stop existing |
| local `ghostty` hook | the config path is inside `entry:` |
| local `pi` hook | the path is in `entry:` and in `SETTINGS = Path(...)` in the script |
| `check-symlinks` | 8 of 10 symlinks become plain text files, and nothing validates their contents |
| `check-executables-have-shebangs` | modes move into filenames, so it finds no executables at all |

### Templating silently removes files from validation

This is the sharper cost. Hook file selection is extension-based.

- `shellcheck` selects on `.sh`. A `run_onchange_install_packages.sh.tmpl` has extension
  `.tmpl`, is not tagged as shell, and stops being checked with no error. Forcing it does
  not help, because `{{ if eq .chezmoi.os "darwin" }}` is not valid shell.
- `pretty-format-json` and the `check-jsonschema` hook likewise skip any `.json.tmpl`. A
  templated JSON file is not JSON, so you do not exclude it, you lose validation of it.

CI here validates rendered configs, and a chezmoi source state is not a rendered config.
The mitigation is a render-then-lint hook plus a `macos-latest` CI leg, because
`chezmoi execute-template` fills `.chezmoi.os` from the runner and a Linux runner only
ever exercises the Linux branch. Real, but real added cost.

### `.ls-lint.yml` collides with chezmoi's naming vocabulary

The repo lints filenames: `snake_case` for `.sh`, `.bash`, `.zsh`, and `kebab-case` for
`.json`, `.toml`, `.yaml`, `.yml`.

- `modify_settings.json` has basename `modify_settings`, which fails `kebab-case`. Same
  for any `private_*.json`.
- `run_once_after_configure-macos.sh` mixes `_` and `-`, which fails `snake_case`.
  chezmoi's own docs use hyphens in script names.
- `.sh.tmpl` matches no rule, so templated scripts lose filename linting entirely.

The fix is to relax `.json` to a regex rule, which means weakening the check.

### The live-edit loop and the `exact_` foot-gun

Today `vim ~/.zshrc` *is* editing `~/.dotfiles/zsh/.zshrc`, and `git diff` shows it. In
chezmoi's default copy mode you need `chezmoi edit --apply`, or `chezmoi re-add`
afterwards, in both directions. That matters more here than in most repos, because
`pi/.pi/agent/settings.json` is rewritten by pi itself and was touched 22 times in a
recent 12-day window. Under Stow those writes land in the repo for free.

`mode = "symlink"` recovers this, but only for files that are neither templated nor
attribute-prefixed. So it restores Stow behavior for the 121 files that never needed
chezmoi and leaves copy semantics on exactly the handful that motivated the migration.

Separately, one `exact_` prefix on `dot_profile.d/`, `dot_zshrc.d/`,
`private_dot_ssh/config.d/`, or `dot_claude/skills/` makes `chezmoi apply` delete the
corp drop-ins that live there, on every run. The default is the tolerant behavior we
want, and nothing warns you. It would have to be a documented prohibition.

### The corp split argues against it

`~/.dotfiles` and `~/.dotfiles_corp` are two independent Stow sources targeting one
`$HOME`, each separately inspectable with `git status` and separately unstowable.
`packages_no_folding := "claude espanso"` exists so `~/.claude/skills` stays a real
directory both repos can write into. Every seam is tool-native:
`[include] path = .gitconfig_corp`, `%include .hgrc_corp`,
`if "[ -f ~/.tmux.conf_corp ]"`, `Include config.d/*`, and the `.profile.d`, `.zshrc.d`,
`.bashrc.d` drop-in directories.

chezmoi is architecturally single-source. `--source` replaces the source directory, it
does not layer, and there is no overlay stack. A second instance for corp means a second
config file, a second source directory, and a separate `--persistent-state` database,
and each instance's `chezmoi status` treats the other's files as noise.

Tool-native seams also keep working without the manager. `Include config.d/*` works on a
box where the repo was never cloned. A `.tmpl` is inert without the chezmoi binary and
its source directory.

The one clean win here: `--no-folding` becomes unnecessary, because chezmoi always
creates real directories.

## Decision

Do not migrate. Keep Stow, the Justfile, and tool-native include seams.

This is specific to this repo, not a judgement about chezmoi. The costs are concentrated
in the two distinguishing features of this setup, an unusually path-coupled lint stack
and a two-repo public/corp split, while the benefit is concentrated in 4% of the files.

[ADR-0001](0001-nix-home-manager-migration.md) is precedent: a careful migration plan that
was never executed, whose own conclusion was that the lighter subset carried most of the
value. The same instinct applies here, and the useful subset is listed below.

### What would flip this decision

1. **Ephemeral or semi-trusted hosts.** `sh -c "$(curl -fsLS get.chezmoi.io)" -- init
   --apply thiagowfx` needs only `curl`, against `git clone --recurse && just install`,
   which needs git, perl and stow, just, and bash 4 or newer. Stock macOS `/bin/bash` is
   3.2. This is the most plausible trigger.
2. **Windows, or anywhere symlinks are second-class.** Stow is a non-starter there.
3. **Five or more heterogeneous machines.** Two machines with a clean corp split is the
   sweet spot for tool-native includes.
4. **Secrets belonging in the public repo.** chezmoi's age and 1Password integration wins
   outright; Stow has no answer. Today those secrets correctly live in
   `~/.dotfiles_corp`.
5. **Merging corp into this repo.** That removes the strongest structural argument above.
6. **Templated values crossing roughly 15-20 across roughly 10 files.** Tool-native seams
   scale in mechanisms-to-know; templates scale in one mechanism. There is a crossover.

### What to steal instead

Ranked by value divided by effort. None of these are implemented yet.

1. **A `just doctor` drift check.** The one real capability gap. `chezmoi status` and
   `chezmoi verify` exist because the copy model creates drift. Stow's drift surface is
   much smaller but not zero: an app that rewrites atomically (write-temp plus rename,
   which VS Code and most Electron apps do) silently replaces the symlink with a regular
   file, and `chkstow` only finds dangling links, not replaced ones. Three layers, about
   25 lines: `stow -n -v` (the closest analogue to `chezmoi diff`), `chkstow -t` and
   `chkstow -a`, and a loop asserting every tracked file resolves back into the repo.
2. **Pin and checksum `just sync-upstream`.** It curls six files from branch HEADs with no
   integrity check, in a repo that freezes every prek hook to a SHA, pins every pi npm
   package with a custom enforcement hook, and lints its Actions with zizmor. One of those
   files, `vendor/grml-etc-core/etc/zsh/zshrc`, is symlinked to `~/.zshrc` and executes on
   every shell start. Change the map from `path → url` to `path → url + sha256` and verify
   after download. This is the good idea inside `.chezmoiexternal.toml`. While there,
   `vendor/ghostty-cursor-shaders` is a whole submodule for one `.glsl` file and is a
   natural `sync-upstream` entry.
3. **`run_onchange_`-style input hashing for `bootstrap`.** `just install` re-runs a full
   `brew bundle` and a sudo-prompting `defaults write` block every time. Both are
   idempotent, so this buys speed and one fewer sudo prompt, not correctness. Hash the
   inputs into `${XDG_CACHE_HOME:-$HOME/.cache}/dotfiles/<recipe>.sha256` and skip when
   unchanged.
4. **`chmod 700 ~/.ssh` in the `stow` recipe.** The entire declarative-permissions gap for
   this repo, in two lines. Exec bits already work, because git tracks mode 0755 and the
   symlink resolves to the repo file. `~/.ssh/config` at 0644 is fine, since ssh only
   rejects a group- or world-writable config and no private keys live here. The only real
   exposure is a fresh machine where Stow creates `~/.ssh` itself.

Deliberately not worth stealing: templating (4% of files, and it costs schema validation),
copy-mode apply, source-name attributes, and `.chezmoiignore`. The `package_binaries`
probe array is the better version of that last idea, because it is capability-based and
prints what it skipped.

## Alternatives Considered

- **Full chezmoi migration** (analysed above): gains templating, declarative modes, secret
  retrieval, and a one-binary bootstrap. Costs the package grouping layer, a day of lint
  rework, schema validation on every templated file, the live-edit loop, and the corp
  overlay.
- **chezmoi in `mode = "symlink"`**: keeps the live-edit loop, but only for files that are
  neither templated nor attribute-prefixed, which excludes every file that motivated the
  move.
- **Nix and Home Manager**: rejected separately in
  [ADR-0001](0001-nix-home-manager-migration.md).
- **Status quo plus targeted fixes** (chosen): Stow, tool-native includes, and the four
  follow-ups above.

## Consequences

- Stow packages, the `package_binaries` capability probe, and the lint stack's hardcoded
  paths all stay as they are.
- The public/corp split keeps working through tool-native includes, and both repos remain
  independently inspectable.
- Editing a file in `$HOME` keeps editing the repo file, and apps that rewrite their own
  config keep writing straight into the repo.
- Per-machine variance keeps being expressed as `Match exec` probes, `command -v` guards,
  and runtime path detection instead of templates.
- Secrets cannot live in this repo. They stay in `~/.dotfiles_corp`.
- Bootstrap keeps needing git, stow, just, and bash 4 or newer, which blocks a
  curl-only install on a stock macOS host.
- Drift caused by atomic-rewrite apps stays undetected until `just doctor` exists.

### The bugs

Auditing for "what would templating fix?" surfaced seven defects that had nothing to do
with the dotfile manager. All are fixed.

<!-- markdownlint-disable MD013 -->

| File | Bug |
| --- | --- |
| `claude/.claude/hooks/notify.sh` | `afplay` ran unguarded on Linux; the guard already existed in the sibling `pi/.pi/agent/extensions/notify.ts` |
| `profile/.profile.d/10_brew.sh` | hardcoded `/opt/homebrew`, though the file's own comment documented the Intel `/usr/local` case; no Linuxbrew fallback |
| `ssh/.ssh/config` | macOS-only 1Password socket hardcoded, Linux path present only as a comment; now two `Match exec` probes |
| `Justfile` (`unstow`) | omitted `packages_no_folding`, so `claude` and `espanso` stayed linked |
| `Justfile` (`swiftbar`) | the loops warned about packages without a mapping but never the reverse; `swiftbar` is a known placeholder, so it warns rather than fails |
| `Justfile` (`espanso`) | gated on the `espanso` binary, which exists on Linux, but the package only ships the macOS `Library/…` path |
| `logseq/.logseq/preferences.json` | contained two different usernames (`/Users/thiago.perrotta` and `/Users/tperrotta`); write-through captured each machine's state into a different theme key. Dropped the active-selection pointer only; the theme stays declared and version-pinned in `logseq/.logseq/config/plugins.edn` |

<!-- markdownlint-enable MD013 -->

Seven bugs, zero of which needed chezmoi. That is the argument in miniature.

### Known gaps, not fixed

- `ssh/.ssh/config` has `Include config.d/*` at the end, after `Host *`. ssh_config takes
  the first obtained value, so nothing in `config.d/`, including corp drop-ins, can
  override a `Host *` directive. Possibly deliberate; moving it changes corp behavior.
- `jj/.config/jj/config.toml` has no corp include, where git and hg both do. jj supports
  `~/.config/jj/conf.d/`; filling this needs a corp-side change too.
- `tmux/.tmux.conf` settles on `screen-256color` as a macOS and Linux compromise. The
  comments suggest this is deliberate; the real fix is installing `tmux-256color`
  terminfo, not a config change.

## Verification

If this is ever revisited, the migration is testable without committing to it:

```bash
chezmoi --source=home --destination="$(mktemp -d)" init --apply --no-tty
chezmoi --source=home --destination="$(mktemp -d)" verify
chezmoi --source=home execute-template < home/.chezmoiignore   # compare against `just stow`
```

The last command is the cheapest way to check the gating port. Its output should match the
"Stowing / Skipping" log that the current `stow` recipe prints.

[chezmoi]: https://www.chezmoi.io/
