---
name: bloggify
description: Draft a blog post for perrotta.dev in the existing house style. Use when the user wants to turn something they just did, learned, or fixed into a post for the blog, or mentions bloggify, "write a blog post", or perrotta.dev.
argument-hint: "<topic or what you want to write about>"
model: sonnet
allowed-tools: Bash(just new:*), Bash(just blog:*), Bash(just touch:*), Bash(date:*), Bash(ls:*), Bash(cat:*), Bash(grep:*), Bash(rg:*), Bash(find:*), Bash(git -C:*), Bash(prek run:*), Read, Edit, Write, Glob
---

Write a blog post about `$ARGUMENTS` for the perrotta.dev blog.

## The blog

- Repo root: `~/Workspace/perrotta.dev`. Hugo-based. Posts live in `content/posts/` as `YYYY-MM-DD-title.md`.
- The repo owns the style. **Read `~/Workspace/perrotta.dev/STYLE.md` and `~/Workspace/perrotta.dev/CLAUDE.md` first** — they are the source of truth for voice, structure, frontmatter, and conventions. Do not duplicate or override them here; follow them.
- Read the 3–4 most recent posts to calibrate. Get them by date prefix, not mtime:
  ```sh
  ls ~/Workspace/perrotta.dev/content/posts/*.md | sort | tail -5
  ```

## Process

### Step 1 — Get the material, not just the topic

The house style is *show, don't explain*: real commands, real output, real diffs, real file paths — code is 50–80% of the post. A topic alone is not enough.

- If the user just did the thing in this session (a fix, a command, a tool they built), pull the actual artifacts: the command they ran and its output, the diff, the commit shortlog, the upstream issue/PR link.
- If the raw material isn't in hand, ask the user for it (paste the terminal output / point at the repo / link the PR) rather than inventing toy examples. Sanitized fake examples are the one thing the style forbids.
- Strip secrets/tokens from pasted output, but keep it real otherwise (real hashes, paths, versions).

### Step 2 — Scaffold the file

```sh
cd ~/Workspace/perrotta.dev && just new "post title"
```

This creates `content/posts/$(date +%F)-<slug>.md` with frontmatter and (when run in a TTY) opens the editor — here it just prints the path. The title is lowercased per house style; namespace with a colon when it fits (e.g. `kubectl: atomic secret upsert`).

If you need to set the date precisely, `just touch <path>` rewrites the frontmatter `date:` and the filename prefix to now.

### Step 3 — Write it

Follow STYLE.md. The shape, in brief (defer to STYLE.md for detail):

- Minimal frontmatter: `title`, `date`, `tags` only. Pick tags from the existing vocabulary (`ai`, `coding`, `dev`, `linux`, `macos`, `meta`, `privacy`, `serenity`, `terraform`, …) — grep existing posts rather than inventing new ones.
- Open with a bold **Problem statement**, a **Today I learned**, a `[Previously]({{< ref "..." >}}).` backlink, or jump straight in. Never "In this post, I will…".
- Keep it short (most posts < 50 lines). Let the code blocks carry the weight.
- `%` prompt for zsh, `$` for bash. `>` blockquotes for quoting upstream docs verbatim.
- Internal cross-refs use `{{< ref "YYYY-MM-DD-slug" >}}` (slug only, no path, no `.md`). If you reference an earlier post, verify the slug exists in `content/posts/`.
- Close with one sentence of reflection, an upstream link, or nothing. Never "In conclusion…".

### Step 3.5 — Disclose that this skill wrote the draft

Transparency matters (see `2025-03-26-disclosing-ai-in-pull-requests` — the established signal is a 🤖). End every post this skill drafts with a subtle footer, separated from the body by a thematic break:

```markdown
- - -

🤖 *Drafted with `/bloggify`.*
```

Keep it to that one italic line — subtle, not a disclaimer block. Do not remove or soften it; the user can edit it out themselves if they ever want to, but the default is full disclosure.

### Step 4 — Lint and show

```sh
cd ~/Workspace/perrotta.dev && prek run --files content/posts/<file>
```

Markdownlint and the Hugo build run here. Fix what it flags. Then show the user the rendered file path and the post body, and stop — **don't commit or push** unless asked. Committing is the user's call (`git commit -m "new post: title"`).

## Notes

- This skill drafts; the user edits and ships. Hand back a draft that reads like their other posts, not a finished decree.
- The blog's own CLAUDE.md states no AI assistance is used for content — this skill exists because the user explicitly asked for it. Treat output as a *draft for the user*, leave the final voice to them, disclose it (Step 3.5), and don't commit on your own.
