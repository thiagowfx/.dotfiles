# obsidian

Reference copies of per-vault Obsidian settings. Obsidian has no global
settings location — everything lives in `<vault>/.obsidian/` — so these are
plain copies, not live symlinks (symlinks inside an iCloud vault don't sync to
iOS). Stow places them at `~/.obsidian.d/` for convenience.

Seed a vault:

```sh
cp ~/.obsidian.d/*.json "<vault>/.obsidian/"
```

Pull changes back from a vault:

```sh
cp "<vault>/.obsidian/"{app,appearance,backlink,core-plugins,daily-notes}.json ~/.dotfiles/obsidian/.obsidian.d/
```

Deliberately excluded: `workspace.json`, `graph.json`, `bookmarks.json`
(per-device UI state) and `themes/` (re-downloadable in-app).

`~/.obsidian.d` and not `~/.config/obsidian` because the latter is Obsidian's
Electron app-data dir on Linux.
