import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

const PHASES = ["🌕", "🌔", "🌓", "🌒", "🌑"] as const;

export function moonForPercent(percent: number): (typeof PHASES)[number] {
  const clamped = Math.max(0, Math.min(100, percent));
  return PHASES[Math.min(PHASES.length - 1, Math.floor(clamped / 20))];
}

export default function (pi: ExtensionAPI) {
  function updateStatus(ctx: ExtensionContext) {
    const usage = ctx.getContextUsage();
    if (!usage) {
      ctx.ui.setStatus("context-moon", undefined);
      return;
    }

    if (usage.percent === null) {
      ctx.ui.setStatus("context-moon", ctx.ui.theme.fg("dim", "[🌙 ?]"));
      return;
    }

    const percent = Math.max(0, Math.min(100, usage.percent));
    const color = percent >= 75 ? "error" : percent >= 50 ? "warning" : "success";
    ctx.ui.setStatus(
      "context-moon",
      ctx.ui.theme.fg(color, `[${moonForPercent(percent)} ${percent.toFixed(0)}%]`),
    );
  }

  pi.on("session_start", async (_event, ctx) => updateStatus(ctx));
  pi.on("model_select", async (_event, ctx) => updateStatus(ctx));
  pi.on("before_agent_start", async (_event, ctx) => updateStatus(ctx));
  pi.on("message_end", async (_event, ctx) => updateStatus(ctx));
  pi.on("session_compact", async (_event, ctx) => updateStatus(ctx));
}
