import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  // Alias for the built-in /new command.
  pi.registerCommand("clear", {
    description: "Start a new session (alias for /new)",
    handler: async (_args, ctx) => {
      await ctx.newSession({
        parentSession: ctx.sessionManager.getSessionFile(),
      });
    },
  });

  // Alias for the built-in /name command.
  pi.registerCommand("rename", {
    description: "Set session display name (alias for /name)",
    handler: (args, ctx) => {
      const name = args.trim();
      if (!name) {
        const currentName = pi.getSessionName();
        ctx.ui.notify(
          currentName ? `Session name: ${currentName}` : "Usage: /rename <name>",
          currentName ? "info" : "warning",
        );
        return;
      }

      pi.setSessionName(name);
    },
  });
}
