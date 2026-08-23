/**
 * GitHub PR link status
 *
 * Shows the current branch pull request as a clickable `PR #123` statusline entry.
 * Nothing else: no checks state, no review state, no comment count.
 *
 * Refreshes on session start and after each agent turn. One `gh pr view` call per refresh.
 * Any failure (no PR, no `gh`, unauthenticated, not a repository) clears the entry.
 */

import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

const STATUS_KEY = "github-pr";
const GH_TIMEOUT_MS = 10_000;

export function osc8Link(url: string, text: string): string {
	try {
		const parsed = new URL(url);
		if (parsed.protocol !== "http:" && parsed.protocol !== "https:") return text;
		return `\x1b]8;;${parsed.toString()}\x07${text}\x1b]8;;\x07`;
	} catch {
		return text;
	}
}

export function formatPrLink(stdout: string): string | undefined {
	let pr: { number?: unknown; url?: unknown; state?: unknown };
	try {
		pr = JSON.parse(stdout);
	} catch {
		return undefined;
	}
	if (pr.state !== "OPEN") return undefined;
	if (typeof pr.number !== "number" || !Number.isFinite(pr.number)) return undefined;
	const label = `PR #${pr.number}`;
	return typeof pr.url === "string" ? osc8Link(pr.url, label) : label;
}

export default function (pi: ExtensionAPI) {
	let request = 0;

	const refresh = async (ctx: ExtensionContext) => {
		request += 1;
		const current = request;
		let status: string | undefined;
		try {
			const result = await pi.exec("gh", ["pr", "view", "--json", "number,url,state"], {
				cwd: ctx.cwd,
				signal: ctx.signal,
				timeout: GH_TIMEOUT_MS,
			});
			if (result.code === 0 && !result.killed) status = formatPrLink(result.stdout);
		} catch {
			status = undefined;
		}
		if (current === request && !ctx.signal?.aborted) ctx.ui.setStatus(STATUS_KEY, status);
	};

	pi.on("session_start", async (_event, ctx) => refresh(ctx));
	pi.on("agent_end", async (_event, ctx) => refresh(ctx));
	pi.on("session_shutdown", async (_event, ctx) => {
		request += 1;
		ctx.ui.setStatus(STATUS_KEY, undefined);
	});
}
