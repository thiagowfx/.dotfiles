---
name: new-apkbuild
description: Scaffold and iterate on an Alpine Linux APKBUILD for a new aport. Use when packaging something for Alpine, working under the testing tree, or the user mentions APKBUILD, aports, or newapkbuild.
argument-hint: "<pkgname> [reference-url]"
allowed-tools: Bash(ls:*), Bash(test:*), Bash(cat:*), Bash(grep:*), Bash(find:*), Bash(rg:*), Bash(newapkbuild:*), Bash(abuild:*), Bash(abuild-sign:*), Bash(apkbuild-lint:*), Bash(secfixes-check:*), Bash(git status:*), Bash(git diff:*), Bash(git log:*), Bash(git add:*), Bash(git commit:*), Bash(git checkout:*), Bash(git rev-parse:*), Bash(git branch:*), Bash(glab:*), WebFetch
---

Package `$ARGUMENTS` for Alpine Linux.

## Context

- Aports tree: a clone of `https://gitlab.alpinelinux.org/alpine/aports` (or a personal fork). Discover the path via `git rev-parse --show-toplevel` from inside the tree.
- New packages go under `testing/<pkgname>/`.
- Alpine uses `doas`, not `sudo` (the `doas-sudo-shim` package makes `sudo` invocations work, but prefer `doas`).
- `license=` must be a valid SPDX identifier (see `spdx-licenses-list`).
- Cross-distro references: AUR (`https://aur.archlinux.org/packages/<name>`), repology.org. Pull the upstream's own build docs too — don't infer dependencies from package names.

## Step 1 — Scaffold

1. If `<aports>/testing/<pkgname>/APKBUILD` already exists, read it and skip to Step 3.
2. Find a sibling aport in the same language/ecosystem for a structural template:
   ```sh
   rg -l "^pkgname=" <aports>/{testing,community,main}/ | xargs grep -l '<distinctive marker e.g. cargo, go build>'
   ```
3. Fetch the AUR PKGBUILD (or upstream build doc) as a starting baseline if a reference URL was provided.
4. Scaffold:
   ```sh
   cd <aports>/testing
   newapkbuild <flags> <pkgname>
   ```
   Language flags: `-r` Rust/cargo, `-y` Python, `-p` Perl, `-c` C autotools, `-e` empty, `-a` C autotools w/ aclocal, `-m` Meson, `-n` `pkgname-pkgver` (no-fetch). Pick the closest match; tweak after.

## Step 2 — Populate APKBUILD

Required fields:
- `pkgname`, `pkgver`, `pkgrel=0` (first version)
- `pkgdesc` (one short sentence, no trailing period, no "the"/"a" leader)
- `url`, `arch="all"` (don't pre-narrow — let CI tell you)
- `license=` SPDX id
- `makedepends`, `checkdepends`, `subpackages` (typical: `$pkgname-doc $pkgname-openrc`)
- `source="$pkgname-$pkgver.tar.gz::<stable-upstream-url>"`
- `build()`, `check()`, `package()`

Don't invent dependencies. Each entry must trace to upstream's build/test instructions or the reference PKGBUILD.

## Step 3 — Iterate

```sh
abuild checksum            # SHA512 for $source
abuild unpack && ls src/   # confirm tree layout matches build() assumptions
abuild -r                  # clean rootbld build; outputs to ~/packages/testing/
apkbuild-lint APKBUILD
abuild -r lint
```

`abuild` requires an Alpine environment. If the current host isn't Alpine, stop and ask the user to run these steps; don't fabricate output.

## Step 4 — Common gotchas

- **Rust**: if integration tests pull in unavailable runtimes (docker, julia, lua, haskell, node, ruby, swift, bun), restrict to unit tests:
  ```
  check() { cargo test --frozen --lib; }
  ```
  Add `checkdepends="bash python3 ..."` only for runtimes the unit tests actually need.
- **Go**: `-buildmode=pie` may require `export CGO_ENABLED=1` (e.g. `make CGO_FLAG=1`). Watch for `math.MaxInt64` int-overflow errors on 32-bit archs — guard with `// +build !386 !arm` or restrict `arch=`.
- **Test failures usually mean missing `checkdepends`** (bash, python3, git, …) before they mean "skip the test".
- **Don't sign locally** unless the user has a key configured; CI signing is fine.
- **`arch=`**: start with `"all"`. Only narrow after CI confirms which archs genuinely can't build.

## Step 5 — CI across all archs

Push a branch (`new-aport-<pkgname>` or similar) to the fork. GitLab CI runs the build matrix. When the pipeline finishes, smoke-test the produced `.apk`:
```sh
# from a pipeline artifact
doas apk add --allow-untrusted ./<pkgname>-<pkgver>-r0.apk
<pkgname> --version
```

## Step 6 — Submit

- Commit message: `testing/<pkgname>: new aport` (Alpine convention; one logical change per commit).
- Open a GitLab MR against `alpine/aports`. `glab mr create` if available, otherwise the web UI.
- Wait for the cross-arch pipeline. Iterate on Step 3 until green.

## Output

When done in one turn, report: the APKBUILD path, the pipeline URL (if pushed), and any arch/test that needed special handling — nothing else.
