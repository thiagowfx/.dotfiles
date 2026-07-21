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
}
