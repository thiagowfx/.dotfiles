# ADR-0001: Nix and Home Manager

## Status

Abandoned

## Date

2026-02-23

## Context

The repo manages dotfiles with GNU Stow (symlinks), Homebrew (packages), and a Justfile
(orchestration and platform gating). This works, but it is imperative: `just install`
runs a sequence of steps, and nothing compares the result against a declared target
state.

Nix with [Home Manager](https://github.com/nix-community/home-manager) and
[nix-darwin](https://github.com/LnL7/nix-darwin) would make the whole setup
declarative. One command, `darwin-rebuild switch --flake .`, would build a known
environment from scratch, with atomic rollback.

A full migration plan was written. Its shape:

- `flake.nix` at the repo root, `nixpkgs` pinned by `flake.lock`, one module per tool
  under `nix/home/`, and platform modules under `nix/platform/{darwin,linux}.nix`.
- Hybrid dotfile handling. Most configs stay raw files referenced through `home.file` or
  `xdg.configFile`. Only tools whose Home Manager module adds value (git, ssh, fzf,
  direnv, zoxide) become Nix expressions.
- About 130 Brewfile formulae move to `home.packages`. About 48 GUI casks stay in
  Homebrew but are declared through the nix-darwin `homebrew` module. Formula-only
  packages (`mas`, `macos-trash`, `folderify`, the `atlassian/acli/acli` tap) stay in
  Homebrew.
- `programs.zsh` replaces `grml-etc-core`. The `zsh-autosuggestions` and
  `zsh-syntax-highlighting` submodules become Home Manager options. The
  `.profile.d` and `.zshrc.d` drop-in pattern survives unchanged.
- The `configure-macos` Justfile recipe becomes nix-darwin `system.defaults`, applied on
  every rebuild instead of once, plus Touch ID sudo through
  `security.pam.services.sudo_local.touchIdAuth`.
- Migration in seven incremental phases, because Stow and Nix coexist: scaffold,
  packages, simple configs, complex configs, shell, platform, cleanup. One package at a
  time, unstow then rebuild.

The plan was never executed.

## Decision

Do not migrate. Keep Stow, Homebrew, and the Justfile.

The declarative benefits are real, but the cost lands on parts of the setup that already
work:

- Nix is a language to learn, and its error messages are hard to read.
- `/nix/store` uses 5-20 GB of disk.
- Live edits stop working. `git config --global` can no longer edit the gitconfig,
  because the source of truth is a Nix expression that must be rebuilt.
- Rebuild takes 5-30 s, against instant Stow symlinks.
- Some Homebrew packages have no `nixpkgs` equivalent and need custom derivations.
- macOS upgrades sometimes break Nix.
- Total config size grows from a 244-line Justfile to an estimated 500-1000 lines of Nix.
- Homebrew stays anyway, for GUI casks.

The plan's own closing section names the cheaper option: use Nix only for package
management and keep Stow for symlinks, for "about 60% of the benefit for about 20% of the
effort". That option is recorded here as open, not adopted. It removes the Brewfile but
keeps every other seam intact.

## Alternatives Considered

- **Full nix-darwin plus Home Manager** (the plan above): highest reproducibility, and
  the only option that makes macOS defaults declarative. Also the highest cost, and it
  rewrites configs that have no defect.
- **Nix for packages only, Stow for dotfiles**: removes the Brewfile and pins package
  versions, without touching any config file. The best value per unit of effort, and
  still open.
- **Standalone Home Manager without nix-darwin**: no `system.defaults` and no cask
  management, so it pays the Nix learning cost and loses the platform half of the
  benefit.
- **Status quo** (chosen): Stow plus Homebrew plus Justfile.

## Consequences

- `just install` and `just stow` remain the entry points. Setup stays imperative, and
  there is no rollback command.
- Homebrew keeps managing both formulae and casks. Package versions float.
- `configure-macos` stays a one-shot recipe that must be re-run by hand.
- The `grml-etc-core`, `zsh-autosuggestions`, and `zsh-syntax-highlighting` submodules
  stay.
- Editing `~/.zshrc` still edits the repo file directly, and `git diff` shows it.
- Drift between the repo and `$HOME` is not detected. A `just doctor` check is the
  narrow replacement for `darwin-rebuild`-style verification.
- Reconsider if the machine count grows, if a from-scratch rebuild becomes frequent, or
  if floating Homebrew versions cause a real failure.
