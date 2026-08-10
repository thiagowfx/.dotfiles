import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

export function updateSessionIdStatus(ctx: ExtensionContext): void {
	const sessionId = ctx.sessionManager.getSessionId();
	ctx.ui.setStatus("session-id", ctx.ui.theme.fg("dim", `sid:${sessionId}`));
}

export default function (pi: ExtensionAPI) {
	pi.on("session_start", async (_event, ctx) => updateSessionIdStatus(ctx));
}
