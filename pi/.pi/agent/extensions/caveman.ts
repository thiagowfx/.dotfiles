import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

// Caveman mode — terse, filler-free responses. Adapted from
// https://github.com/JuliusBrussee/caveman (MIT), reimplemented as a native
// pi extension since pi isn't one of the upstream installer's targets.
//
// Auto-enabled every session. Toggle with /caveman [on|off].

const BANNED_PHRASES = [
  "smoking gun",
  "load bearing",
  "load-bearing",
  "honest opinion",
  "just say the word",
  "I have the full picture",
  "you're right to push back",
  "shapes the entire conversation",
  "invariant violation",
];

const INSTRUCTIONS = `Caveman mode: on. Respond terse. Drop articles (a/an/the), filler (just/really/basically), pleasantries, hedging. Fragments OK. Short synonyms. Technical terms exact. Pattern: [thing] [action] [reason]. [next step]. Example — not: "Sure! I'd be happy to help you with that." yes: "Bug in auth middleware. Fix:"
Write like ASD-STE100 Simplified Technical English: one word, one meaning, one job. Short sentences, active voice, one idea per sentence. No idioms, no metaphors, no hedge words.
Never use these phrases, or close variants: ${BANNED_PHRASES.map((p) => `"${p}"`).join(", ")}.
Boundaries, always: code blocks, commit messages, and PR descriptions stay normal full verbosity, never compressed. Drop caveman style for security warnings, irreversible/destructive actions, or when the user seems confused — resume caveman after.`;

export default function (pi: ExtensionAPI) {
  let on = true; // auto-on by default, every session

  pi.on("before_agent_start", (event) => {
    if (!on) return;
    return { systemPrompt: `${event.systemPrompt}\n\n${INSTRUCTIONS}` };
  });

  pi.registerCommand("caveman", {
    description: "Toggle caveman terse-response mode: on|off (default: on)",
    getArgumentCompletions: (prefix) => {
      const items = ["on", "off"].map((v) => ({ value: v, label: v }));
      const filtered = items.filter((i) => i.value.startsWith(prefix.toLowerCase()));
      return filtered.length > 0 ? filtered : null;
    },
    handler: async (args, ctx) => {
      const arg = args.trim().toLowerCase();
      if (arg === "off" || arg === "stop" || arg === "normal") {
        on = false;
      } else if (arg === "on" || arg === "") {
        on = true;
      } else {
        ctx.ui.notify(`Unknown arg "${arg}". Use: on | off`, "error");
        return;
      }
      ctx.ui.notify(on ? "Caveman mode on." : "Caveman mode off. Normal verbosity.", "info");
    },
  });
}
