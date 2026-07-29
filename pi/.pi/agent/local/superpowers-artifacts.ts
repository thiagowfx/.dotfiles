import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

const artifactInstructions = `## Superpowers artifacts

- Never create or commit \`docs/superpowers/\` in any repository.
- Store every Superpowers design spec and implementation plan in active Obsidian vault through Obsidian CLI.
- Derive \`<owner>/<repo>\` from current repository's GitHub \`origin\`. Accept HTTPS and SSH GitHub URLs, remove the \`.git\` suffix, and strip leading dots from owner and repository names (\`thiagowfx/.dotfiles\` → \`thiagowfx/dotfiles\`). Fail when current directory is not a Git repository, \`origin\` is missing or non-GitHub, or identity is empty after normalization.
- Use vault-relative paths \`superpowers/<owner>/<repo>/specs/YYYY-MM-DD-<topic>-design.md\` and \`superpowers/<owner>/<repo>/plans/YYYY-MM-DD-<topic>.md\`.
- Verify active vault access with \`obsidian vault info=path\`. Create, read, and revise artifacts with \`obsidian create|read\`; use \`overwrite\` only when revising same artifact. If Obsidian CLI or active vault access fails, stop without creating repository-local fallback.
- Before creating or resuming artifact, search qmd collection \`superpowers\`; retrieve full candidates with \`qmd get\` before relying on them.
- After artifact creation or revision, run \`qmd update\` and \`qmd embed -c superpowers\`. qmd is derived index: if refresh fails, preserve Obsidian artifact, report stale indexing, and continue.
- When reporting created or revised artifact, include both its complete vault-relative path in inline code and Markdown link \`[Open <type> in Obsidian](obsidian://open?file=<URL-encoded-path>)\`. URL-encode \`file\` query value. Emit Markdown links, not raw OSC 8 control sequences. External artifacts never belong in source commits.`;

export default function superpowersArtifacts(pi: ExtensionAPI) {
  pi.on("before_agent_start", (event) => ({
    systemPrompt: `${event.systemPrompt}\n\n${artifactInstructions}`,
  }));
}
