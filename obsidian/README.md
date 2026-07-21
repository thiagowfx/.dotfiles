# obsidian

Reference copies of per-vault Obsidian settings — documentation, not stowed.
Obsidian has no global settings location (everything lives in
`<vault>/.obsidian/`), and symlinks inside an iCloud vault don't sync to iOS,
so these are seeded by copying.

Seed a vault:

```sh
cp ~/.dotfiles/obsidian/*.json "<vault>/.obsidian/"
```

Pull changes back from a vault:

```sh
cp "<vault>/.obsidian/"{app,appearance,backlink,core-plugins,daily-notes}.json ~/.dotfiles/obsidian/
```

Deliberately excluded: `workspace.json`, `graph.json`, `bookmarks.json`
(per-device UI state) and `themes/` (re-downloadable in-app).
