import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

export const MCP_STATUS_EVENT = "pi-mcp-adapter/status/v1";

interface McpServerStatus {
	name: string;
	disabled: boolean;
}

interface McpStatusSnapshot {
	version: number;
	servers: McpServerStatus[];
}

function isMcpStatusSnapshot(value: unknown): value is McpStatusSnapshot {
	if (!value || typeof value !== "object") return false;
	const snapshot = value as Partial<McpStatusSnapshot>;
	return snapshot.version === 1
		&& Array.isArray(snapshot.servers)
		&& snapshot.servers.every((server) =>
			server !== null
			&& typeof server === "object"
			&& typeof server.name === "string"
			&& typeof server.disabled === "boolean"
		);
}

export function getEnabledServerNames(snapshot: McpStatusSnapshot): string[] {
	return snapshot.servers
		.filter((server) => !server.disabled)
		.map((server) => server.name)
		.sort((left, right) => left.localeCompare(right));
}

export function formatMcpStartupBlock(ctx: ExtensionContext, serverNames: string[]): string {
	const heading = ctx.ui.theme.fg("mdHeading", "[MCP]");
	const servers = ctx.ui.theme.fg("dim", `  ${serverNames.join(", ")}`);
	return `${heading}\n${servers}`;
}

export default function (pi: ExtensionAPI) {
	let currentCtx: ExtensionContext | undefined;
	let latestSnapshot: McpStatusSnapshot | undefined;
	let shown = false;

	function showStartupBlock(): void {
		if (shown || currentCtx?.mode !== "tui" || !latestSnapshot) return;
		const serverNames = getEnabledServerNames(latestSnapshot);
		if (serverNames.length === 0) return;
		currentCtx.ui.notify(formatMcpStartupBlock(currentCtx, serverNames), "info");
		shown = true;
	}

	pi.events.on(MCP_STATUS_EVENT, (data) => {
		if (!isMcpStatusSnapshot(data)) return;
		latestSnapshot = data;
		showStartupBlock();
	});

	pi.on("session_start", async (_event, ctx) => {
		currentCtx = ctx;
		shown = false;
		showStartupBlock();
	});

	pi.on("session_shutdown", async () => {
		currentCtx = undefined;
		shown = false;
	});
}
