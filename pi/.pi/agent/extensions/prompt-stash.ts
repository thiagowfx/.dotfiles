import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

// Port of Claude Code's Ctrl+S "stash the current prompt" shortcut.
//
//   ctrl+s        stash the current editor text aside and clear the editor
//   ctrl+shift+s  pop the most recent stash back into the editor
//   /stash        list / pop / clear stashed prompts
//
// Stashes are a LIFO stack persisted in the session (via appendEntry), so they
// survive restarts and /reload. They never enter the LLM context.

const ENTRY_TYPE = "prompt-stash";

type StashState = { stack: string[] };

export default function (pi: ExtensionAPI) {
  const state: StashState = { stack: [] };

  // Rehydrate the stack from persisted entries on load / reload / resume.
  const rehydrate = (ctx: ExtensionContext) => {
    state.stack = [];
    for (const entry of ctx.sessionManager.getEntries()) {
      if (entry.type === "custom" && entry.customType === ENTRY_TYPE) {
        const data = entry.data as { stack?: string[] } | undefined;
        if (data?.stack) state.stack = [...data.stack];
      }
    }
  };

  // Persist the stack and refresh the ambient depth widget above the editor.
  const sync = (ctx: ExtensionContext) => {
    pi.appendEntry(ENTRY_TYPE, { stack: state.stack });
    if (ctx.mode !== "tui") return;
    if (state.stack.length === 0) {
      ctx.ui.setWidget("prompt-stash", []);
      return;
    }
    const top = preview(state.stack[state.stack.length - 1]);
    ctx.ui.setWidget("prompt-stash", [`⎇ ${state.stack.length} stashed · top: ${top}`]);
  };

  const preview = (text: string) => {
    const oneLine = text.replace(/\s+/g, " ").trim();
    return oneLine.length > 48 ? `${oneLine.slice(0, 48)}…` : oneLine;
  };

  pi.on("session_start", async (_event, ctx) => {
    rehydrate(ctx);
    sync(ctx);
  });

  // Auto-unstash: after a turn fully settles, pop the most recent stash back
  // into the editor. Only fills an empty editor so we never clobber text the
  // user has already started typing.
  pi.on("agent_settled", async (_event, ctx) => {
    if (ctx.mode !== "tui") return;
    if (state.stack.length === 0) return;
    if (ctx.ui.getEditorText().trim()) return;
    const popped = state.stack.pop()!;
    sync(ctx);
    ctx.ui.setEditorText(popped);
    ctx.ui.notify(`Unstashed prompt (${state.stack.length} left)`, "info");
  });

  // Stash: push current editor text, clear the editor.
  pi.registerShortcut("ctrl+s", {
    description: "Stash the current prompt",
    handler: async (ctx) => {
      if (ctx.mode !== "tui") return;
      const text = ctx.ui.getEditorText();
      if (!text.trim()) {
        ctx.ui.notify("Nothing to stash", "info");
        return;
      }
      state.stack.push(text);
      sync(ctx);
      ctx.ui.setEditorText("");
      ctx.ui.notify(`Stashed prompt (${state.stack.length} on stack)`, "info");
    },
  });

  // Pop: restore the most recent stash into the editor.
  pi.registerShortcut("ctrl+shift+s", {
    description: "Pop the most recent stashed prompt",
    handler: async (ctx) => {
      if (ctx.mode !== "tui") return;
      const popped = state.stack.pop();
      if (popped === undefined) {
        ctx.ui.notify("Stash is empty", "info");
        return;
      }
      sync(ctx);
      const current = ctx.ui.getEditorText();
      ctx.ui.setEditorText(current.trim() ? `${current}\n${popped}` : popped);
      ctx.ui.notify(`Popped stash (${state.stack.length} left)`, "info");
    },
  });

  pi.registerCommand("stash", {
    description: "List, pop, or clear stashed prompts",
    getArgumentCompletions: (prefix) => {
      const items = [
        { value: "list", label: "list — show stashed prompts" },
        { value: "pop", label: "pop — restore the most recent stash" },
        { value: "clear", label: "clear — drop all stashes" },
      ];
      const filtered = items.filter((i) => i.value.startsWith(prefix));
      return filtered.length > 0 ? filtered : null;
    },
    handler: async (args, ctx) => {
      const sub = args.trim() || "list";

      if (sub === "clear") {
        state.stack = [];
        sync(ctx);
        ctx.ui.notify("Cleared all stashes", "info");
        return;
      }

      if (sub === "pop") {
        const popped = state.stack.pop();
        if (popped === undefined) {
          ctx.ui.notify("Stash is empty", "info");
          return;
        }
        sync(ctx);
        if (ctx.mode === "tui") {
          const current = ctx.ui.getEditorText();
          ctx.ui.setEditorText(current.trim() ? `${current}\n${popped}` : popped);
        }
        ctx.ui.notify(`Popped stash (${state.stack.length} left)`, "info");
        return;
      }

      // list (default)
      if (state.stack.length === 0) {
        ctx.ui.notify("No stashed prompts", "info");
        return;
      }
      const lines = state.stack
        .map((t, i) => `${state.stack.length - i}. ${preview(t)}`)
        .reverse();
      ctx.ui.notify(`Stashed prompts:\n${lines.join("\n")}`, "info");
    },
  });
}
