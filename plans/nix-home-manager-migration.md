# Converting dotfiles to Nix + Home Manager

## Context

The repo currently uses GNU Stow (symlinks) + Homebrew (packages) + Justfile (orchestration). This works well but is imperative — you run `just install` and hope the result matches what you expect. Nix + Home Manager makes the entire setup declarative and reproducible: one command (`darwin-rebuild switch --flake .`) produces a known-good environment from scratch, with atomic rollbacks.

## Proposed Flake Structure

```
.dotfiles/
  flake.nix                     # Entry point
  flake.lock                    # Pinned nixpkgs revision
  nix/
    home/
      default.nix               # Main home config, imports all modules
      packages.nix              # All CLI packages (replaces Brewfile brews)
      shell/
        zsh.nix                 # programs.zsh + profile.d/zshrc.d sourcing
        bash.nix                # programs.bash
        starship.nix
        atuin.nix
      git.nix                   # programs.git + .bin/ scripts
      ssh.nix                   # programs.ssh + config.d/ fragments
      editors/
        neovim.nix              # programs.neovim + xdg.configFile for lua config
        vim.nix                 # programs.vim + vim-plug
      terminals/
        ghostty.nix
        alacritty.nix
        tmux.nix
      tools.nix                 # fzf, direnv, zoxide, ack, jj, lf, etc.
    platform/
      darwin.nix                # nix-darwin system config: macOS defaults, Touch ID sudo
      darwin-homebrew.nix       # Declarative cask management via nix-darwin
      linux.nix                 # i3, sway, x11 (conditional)
    overlays/default.nix        # Custom packages not in nixpkgs
    packages/                   # Derivations for pancake, python-is-python3, etc.
```

## flake.nix Skeleton

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, nix-darwin, ... }: {
    # macOS (primary)
    darwinConfigurations."thiago-mac" = nix-darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [
        home-manager.darwinModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.thiago = import ./nix/home;
        }
        ./nix/platform/darwin.nix
        ./nix/platform/darwin-homebrew.nix
      ];
    };

    # Linux (standalone home-manager, no NixOS required)
    homeConfigurations."thiago@linux" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      modules = [ ./nix/home ./nix/platform/linux.nix ];
    };
  };
}
```

## Key Design Decisions

### 1. Dotfile management: hybrid (raw files + Nix modules)

Most config files stay as raw files, referenced via `home.file` or `xdg.configFile`:

```nix
# Simple tools: just symlink the raw config
home.file.".ackrc".source = ../../ack/.ackrc;
xdg.configFile."ghostty/config".source = ../../ghostty/.config/ghostty/config;
xdg.configFile."nvim" = { source = ../../nvim/.config/nvim; recursive = true; };
```

Tools with good Home Manager modules get converted to Nix expressions where it adds value:

```nix
# Git: Nix expression gives type-checked config + declarative aliases
programs.git = {
  enable = true;
  userName = "Thiago Perrotta";
  aliases = { st = "status"; co = "checkout"; /* ... */ };
  delta = { enable = true; options = { navigate = true; }; };
  extraConfig = { push.autoSetupRemote = true; /* ... */ };
};
```

### 2. Shell: drop grml-zsh-config, use Home Manager's zsh module

Home Manager's `programs.zsh` replaces grml. You already use starship for the prompt (grml's main draw). The `zsh-autosuggestions` and `zsh-syntax-highlighting` submodules become Home Manager options instead.

The `.profilerc` / `src_files` / `.profile.d/` pattern survives as-is — each Nix module contributes its `.profile.d/` snippet via `home.file`, and `initExtra` sources them.

### 3. Packages: Nix for CLI, Homebrew for casks

~130 brew packages move to `home.packages` in Nix. GUI casks (~48) stay in Homebrew but managed declaratively via nix-darwin's `homebrew` module — so `darwin-rebuild switch` also runs `brew bundle` for casks.

A handful of brew-only packages stay in Homebrew: `mas`, `macos-trash`, `folderify`, custom taps like `atlassian/acli/acli`.

### 4. macOS defaults become declarative

The `configure-macos` Justfile recipe becomes nix-darwin `system.defaults`:

```nix
system.defaults.NSGlobalDomain = {
  NSAutomaticPeriodSubstitutionEnabled = false;
  InitialKeyRepeat = 10;
  KeyRepeat = 1;
};
system.defaults.dock.show-recents = false;
security.pam.services.sudo_local.touchIdAuth = true;
```

Applied on every `darwin-rebuild switch` instead of a one-shot script.

### 5. Git submodules

| Submodule | Action |
|-----------|--------|
| zsh-autosuggestions | Remove — Home Manager handles it |
| zsh-syntax-highlighting | Remove — Home Manager handles it |
| grml-etc-core | Remove — replaced by programs.zsh |
| lazy.nvim | Keep — let lazy.nvim self-bootstrap |
| vim-plug | Keep — let vim-plug self-bootstrap |
| ghostty-cursor-shaders | Keep or replace with `fetchFromGitHub` |

## Migration Strategy (Incremental)

Stow and Nix can coexist. Migrate one package at a time: unstow it, add the Home Manager equivalent, rebuild.

### Phase 0 — Scaffold

Install Nix (Determinate Systems installer), create `flake.nix` skeleton, verify `darwin-rebuild switch` works with zero config.

**Tasks:**
- [ ] Install Nix via Determinate Systems installer (`curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install`)
- [ ] Create `flake.nix` with the skeleton above
- [ ] Create `nix/home/default.nix` with minimal config (just `home.stateVersion` and `home.username`)
- [ ] Create `nix/platform/darwin.nix` with minimal config
- [ ] Create `nix/platform/darwin-homebrew.nix` as empty placeholder
- [ ] Run `darwin-rebuild switch --flake .` and verify it succeeds
- [ ] Commit the scaffold

### Phase 1 — Packages

Move Brewfile brews to `home.packages`. Set up `homebrew.casks` in nix-darwin. Verify all CLI tools work.

**Tasks:**
- [ ] Create `nix/home/packages.nix` with all CLI packages from Brewfile
- [ ] Map each `brew` formula to its `pkgs.*` equivalent (some names differ)
- [ ] Identify packages with no nixpkgs equivalent; keep those in Homebrew
- [ ] Populate `nix/platform/darwin-homebrew.nix` with `homebrew.casks` list
- [ ] Add custom taps that must stay in Homebrew
- [ ] Run `darwin-rebuild switch --flake .` and verify all CLI tools are available
- [ ] Verify `which <tool>` resolves to `/nix/store/...` for Nix-managed packages
- [ ] Commit

### Phase 2 — Simple configs

Migrate tools with straightforward configs one by one. For each: unstow, add Nix module, rebuild, verify.

**Tasks (one per tool):**
- [ ] `starship` → `nix/home/shell/starship.nix` (use `programs.starship` or raw config)
- [ ] `atuin` → `nix/home/shell/atuin.nix`
- [ ] `fzf` → include in `nix/home/tools.nix` (use `programs.fzf`)
- [ ] `direnv` → include in `nix/home/tools.nix` (use `programs.direnv`)
- [ ] `zoxide` → include in `nix/home/tools.nix` (use `programs.zoxide`)
- [ ] `ack` → include in `nix/home/tools.nix` (raw config via `home.file`)
- [ ] `jj` → include in `nix/home/tools.nix` (use `programs.jujutsu` or raw config)
- [ ] `lf` → include in `nix/home/tools.nix` (use `programs.lf` or raw config)
- [ ] `gitui` → include in `nix/home/tools.nix` (raw config)
- [ ] `ghostty` → `nix/home/terminals/ghostty.nix` (raw config via `xdg.configFile`)
- [ ] `alacritty` → `nix/home/terminals/alacritty.nix` (use `programs.alacritty` or raw config)
- [ ] Run `darwin-rebuild switch --flake .` after each tool, verify config is active
- [ ] Unstow each migrated package
- [ ] Commit after each tool or in batches

### Phase 3 — Complex configs

Git, SSH, tmux, vim, nvim. These need more careful conversion.

**Tasks:**
- [ ] `git` → `nix/home/git.nix`
  - Convert all aliases (~40+)
  - Convert all config sections (push, pull, merge, diff, etc.)
  - Set up delta integration
  - Handle `.bin/` scripts (symlink via `home.file`)
  - Handle `.gitignore_global`
- [ ] `ssh` → `nix/home/ssh.nix`
  - Convert `config.d/` include pattern
  - Use `programs.ssh` with `matchBlocks` or raw config
- [ ] `tmux` → `nix/home/terminals/tmux.nix`
  - Use `programs.tmux` or raw config via `xdg.configFile`
  - Handle TPM plugins
- [ ] `vim` → `nix/home/editors/vim.nix`
  - Symlink `.vimrc` via `home.file`
  - Keep vim-plug self-bootstrapping
- [ ] `nvim` → `nix/home/editors/neovim.nix`
  - Symlink entire `~/.config/nvim/` via `xdg.configFile` recursive
  - Keep lazy.nvim self-bootstrapping
- [ ] Unstow each migrated package
- [ ] Verify each tool uses the correct config
- [ ] Commit after each tool

### Phase 4 — Shell

The hardest part. Create zsh.nix and bash.nix, migrate the profile/zshrc sourcing pattern, replace grml, remove zsh plugin submodules.

**Tasks:**
- [ ] Create `nix/home/shell/zsh.nix`
  - Enable `programs.zsh`
  - Add `zsh-autosuggestions` and `zsh-syntax-highlighting` via Home Manager
  - Set `initExtra` to source `.profile.d/` and `.zshrc.d/` files
  - Migrate key `.zshrc` settings (history, options, keybindings)
- [ ] Create `nix/home/shell/bash.nix`
  - Enable `programs.bash`
  - Set `initExtra` to source `.profile.d/` files
- [ ] Migrate `.profilerc` / `.profile.d/` files via `home.file`
- [ ] Migrate `.zshrc.d/` files via `home.file`
- [ ] Remove grml-etc-core submodule
- [ ] Remove zsh-autosuggestions submodule
- [ ] Remove zsh-syntax-highlighting submodule
- [ ] Test extensively: open new terminal, verify PATH, aliases, prompt, completions, plugins
- [ ] Commit

### Phase 5 — Platform

macOS defaults via nix-darwin. Linux conditional configs.

**Tasks:**
- [ ] Convert `configure-macos` Justfile recipe to `system.defaults` in `nix/platform/darwin.nix`
  - Keyboard repeat settings
  - Dock preferences
  - Finder preferences
  - Other macOS defaults
- [ ] Add Touch ID sudo via `security.pam.services.sudo_local.touchIdAuth`
- [ ] Create `nix/platform/linux.nix` with conditional i3/sway/x11 configs
- [ ] Verify `darwin-rebuild switch` applies macOS defaults correctly
- [ ] Commit

### Phase 6 — Cleanup

Remove old orchestration, update CI, update docs.

**Tasks:**
- [ ] Remove Justfile stow recipes (keep non-stow recipes like `update`)
- [ ] Remove Brewfile (or keep as reference/backup)
- [ ] Update CI: add `nix flake check` workflow
- [ ] Update README with new setup instructions
- [ ] Update CLAUDE.md with new commands
- [ ] Create `nix/overlays/default.nix` for any custom packages
- [ ] Create derivations in `nix/packages/` for packages not in nixpkgs
- [ ] Final full test: `darwin-rebuild switch --flake .` from scratch
- [ ] Commit

## Trade-offs

**Gets better:**
- Fully reproducible: `darwin-rebuild switch --flake .` from scratch
- Atomic rollbacks: `darwin-rebuild switch --rollback`
- No stow dangling symlinks / collision issues
- Shell plugins managed by Nix (no submodules)
- macOS defaults applied on every rebuild
- Pinned nixpkgs via `flake.lock` (explicit `nix flake update`)
- Per-project dev environments via `nix-direnv`

**Gets harder:**
- Nix language learning curve; error messages are notoriously opaque
- Disk space: /nix/store uses 5-20GB
- Can't `git config --global` to edit gitconfig (must edit Nix source + rebuild)
- Rebuild takes 5-30s vs instant stow
- Some Homebrew packages have no nixpkgs equivalent (need custom derivations)
- macOS updates occasionally break Nix
- More total lines of config (~500-1000 lines of Nix vs ~244 line Justfile)
- Still needs Homebrew for GUI casks

## Lighter Alternative

If the full conversion feels too heavy: use Nix **only for package management** (replace Brewfile), keep Stow for dotfile symlinks. This gives ~60% of the benefit for ~20% of the effort — you get reproducible package installation without rewriting any configs.

## Verification

- `darwin-rebuild switch --flake .` succeeds
- Open a new terminal: verify PATH, aliases, prompt, shell plugins all work
- `git`, `nvim`, `vim`, `tmux`, `ssh` all use the correct configs
- `nix flake check` passes in CI
- `which <tool>` resolves to `/nix/store/...` for Nix-managed packages
