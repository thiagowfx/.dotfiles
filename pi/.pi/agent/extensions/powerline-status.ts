import { basename } from "node:path";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

const ANSI_PATTERN = /\x1b\[[0-?]*[ -/]*[@-~]/g;
const OSC_PATTERN = /\x1b\][^\x07]*(?:\x07|\x1b\\)/g;
const TRUECOLOR_RESET = "\x1b[0m";

type ThinkingColor =
	| "thinkingOff"
	| "thinkingMinimal"
	| "thinkingLow"
	| "thinkingMedium"
	| "thinkingHigh"
	| "thinkingXhigh"
	| "thinkingMax";

type PowerlineColor = "accent" | "dim" | "error" | "muted" | "success" | "text" | "warning" | ThinkingColor;

interface ThemeLike {
	fg(color: PowerlineColor, text: string): string;
}

export interface UsageTotals {
	input: number;
	output: number;
	cacheRead: number;
	cacheWrite: number;
	cost: number;
}

interface ContextUsage {
	tokens: number | null;
	contextWindow: number;
	percent: number | null;
}

export interface PowerlineState {
	modelName: string;
	modelReasoning: boolean;
	thinkingLevel: string;
	cwd: string;
	branch: string | null;
	usage: UsageTotals;
	context: ContextUsage | undefined;
	usingSubscription: boolean;
	statuses: ReadonlyMap<string, string>;
	nerdFonts: boolean;
}

interface Segment {
	content: string;
	width: number;
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === "object" && value !== null && !Array.isArray(value);
}

function numeric(value: unknown): number {
	return typeof value === "number" && Number.isFinite(value) ? value : 0;
}

function usageFrom(value: unknown): UsageTotals | undefined {
	if (!isRecord(value)) return undefined;
	const cost = isRecord(value.cost) ? numeric(value.cost.total) : numeric(value.cost);
	return {
		input: numeric(value.input),
		output: numeric(value.output),
		cacheRead: numeric(value.cacheRead),
		cacheWrite: numeric(value.cacheWrite),
		cost,
	};
}

export function collectUsage(entries: readonly unknown[]): UsageTotals {
	const totals: UsageTotals = { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, cost: 0 };

	for (const entry of entries) {
		if (!isRecord(entry)) continue;
		let usage: UsageTotals | undefined;
		if (entry.type === "message" && isRecord(entry.message)) {
			const role = entry.message.role;
			if (role === "assistant" || role === "toolResult") usage = usageFrom(entry.message.usage);
		} else if (entry.type === "branch_summary" || entry.type === "compaction") {
			usage = usageFrom(entry.usage);
		}
		if (!usage) continue;
		totals.input += usage.input;
		totals.output += usage.output;
		totals.cacheRead += usage.cacheRead;
		totals.cacheWrite += usage.cacheWrite;
		totals.cost += usage.cost;
	}

	return totals;
}

export function formatTokens(count: number): string {
	if (count < 1_000) return count.toString();
	if (count < 10_000) return `${(count / 1_000).toFixed(1)}k`;
	if (count < 1_000_000) return `${Math.round(count / 1_000)}k`;
	if (count < 10_000_000) return `${(count / 1_000_000).toFixed(1)}M`;
	return `${Math.round(count / 1_000_000)}M`;
}

export function visibleWidth(value: string): number {
	return Array.from(value.replace(OSC_PATTERN, "").replace(ANSI_PATTERN, "")).length;
}

export function hasNerdFonts(env: NodeJS.ProcessEnv = process.env): boolean {
	if (env.POWERLINE_NERD_FONTS === "1") return true;
	if (env.POWERLINE_NERD_FONTS === "0") return false;
	if (env.GHOSTTY_RESOURCES_DIR) return true;
	return ["iterm", "wezterm", "kitty", "ghostty", "alacritty"].some((name) =>
		(env.TERM_PROGRAM ?? "").toLowerCase().includes(name),
	);
}

function truecolor(hex: string, text: string): string {
	const value = hex.slice(1);
	const red = Number.parseInt(value.slice(0, 2), 16);
	const green = Number.parseInt(value.slice(2, 4), 16);
	const blue = Number.parseInt(value.slice(4, 6), 16);
	return `\x1b[38;2;${red};${green};${blue}m${text}${TRUECOLOR_RESET}`;
}

function segment(content: string): Segment {
	return { content, width: visibleWidth(content) };
}

function withIcon(icon: string, text: string): string {
	return icon ? `${icon} ${text}` : text;
}

function contextColor(theme: ThemeLike, context: ContextUsage): string {
	const text = context.tokens === null || context.percent === null
		? `?/${formatTokens(context.contextWindow)}`
		: `${formatTokens(context.tokens)}/${formatTokens(context.contextWindow)} (${context.percent.toFixed(1)}%)`;
	if ((context.percent ?? 0) > 90) return theme.fg("error", text);
	if ((context.percent ?? 0) > 70) return theme.fg("warning", text);
	return theme.fg("dim", text);
}

function sanitizeStatus(text: string): string {
	return text.replace(/[\r\n\t]/g, " ").replace(/ +/g, " ").trim();
}

function thinkingColor(level: string): ThinkingColor {
	switch (level) {
		case "minimal": return "thinkingMinimal";
		case "low": return "thinkingLow";
		case "medium": return "thinkingMedium";
		case "high": return "thinkingHigh";
		case "xhigh": return "thinkingXhigh";
		case "max": return "thinkingMax";
		default: return "thinkingOff";
	}
}

function buildSegments(state: PowerlineState, theme: ThemeLike): Segment[] {
	const icons = state.nerdFonts
		? { model: "\uEC19", folder: "\uF115", branch: "\uF126", context: "\uF1C0", cache: "\uF1C0", input: "\uF090" }
		: { model: "", folder: "dir", branch: "⎇", context: "◫", cache: "cache", input: "in:" };
	const segments: Segment[] = [];

	const modelName = state.modelName.startsWith("Claude ") ? state.modelName.slice(7) : state.modelName;
	segments.push(segment(truecolor("#d787af", withIcon(icons.model, modelName))));

	if (state.modelReasoning) {
		const level = state.thinkingLevel === "medium" ? "med" : state.thinkingLevel;
		segments.push(segment(theme.fg(thinkingColor(state.thinkingLevel), `think:${level}`)));
	}

	segments.push(segment(truecolor("#00afaf", withIcon(icons.folder, basename(state.cwd) || state.cwd))));
	if (state.branch) segments.push(segment(theme.fg("success", withIcon(icons.branch, state.branch))));

	if (state.context) {
		segments.push(segment(withIcon(icons.context, contextColor(theme, state.context))));
	}
	if (state.usage.cacheRead > 0) {
		segments.push(segment(theme.fg("muted", [icons.cache, icons.input, formatTokens(state.usage.cacheRead)].filter(Boolean).join(" "))));
	}
	if (state.usage.cost > 0 || state.usingSubscription) {
		const cost = state.usage.cost > 0 ? `$${state.usage.cost.toFixed(3)}` : "";
		segments.push(segment(theme.fg("text", `${cost}${state.usingSubscription ? `${cost ? " " : ""}(sub)` : ""}`)));
	}

	const statuses = [...state.statuses.entries()]
		.sort(([left], [right]) => left.localeCompare(right))
		.map(([, text]) => sanitizeStatus(text))
		.filter((text) => text && visibleWidth(text) > 0);
	if (statuses.length > 0) segments.push(segment(statuses.join(" · ")));

	return segments;
}

function renderRow(segments: readonly Segment[], separator: string, theme: ThemeLike): string {
	if (segments.length === 0) return "";
	return ` ${segments.map((item) => item.content).join(` ${theme.fg("dim", separator)} `)} `;
}

export function renderPowerline(width: number, state: PowerlineState, theme: ThemeLike): string[] {
	if (width < 3) return [];
	const separator = state.nerdFonts ? "\uE0B1" : "|";
	const separatorWidth = visibleWidth(separator) + 2;
	const rows: Segment[][] = [[], []];
	let row = 0;
	let rowWidth = 2;

	for (const item of buildSegments(state, theme)) {
		const addedWidth = item.width + (rows[row].length > 0 ? separatorWidth : 0);
		if (rowWidth + addedWidth <= width) {
			rows[row].push(item);
			rowWidth += addedWidth;
			continue;
		}
		if (row === 0) {
			row = 1;
			rowWidth = 2;
		}
		const secondAddedWidth = item.width + (rows[row].length > 0 ? separatorWidth : 0);
		if (rowWidth + secondAddedWidth > width) break;
		rows[row].push(item);
		rowWidth += secondAddedWidth;
	}

	return rows.map((items) => renderRow(items, separator, theme)).filter(Boolean);
}

function stateFrom(ctx: ExtensionContext, footerData: {
	getGitBranch(): string | null;
	getExtensionStatuses(): ReadonlyMap<string, string>;
}): PowerlineState {
	const model = ctx.model;
	return {
		modelName: model?.name || model?.id || "no-model",
		modelReasoning: model?.reasoning === true,
		thinkingLevel: ctx.thinkingLevel || "off",
		cwd: ctx.cwd,
		branch: footerData.getGitBranch(),
		usage: collectUsage(ctx.sessionManager.getEntries()),
		context: ctx.getContextUsage(),
		usingSubscription: model
			? model.provider === "kimi-coding" || ctx.modelRegistry.isUsingOAuth(model)
			: false,
		statuses: footerData.getExtensionStatuses(),
		nerdFonts: hasNerdFonts(),
	};
}

export default function (pi: ExtensionAPI) {
	let currentCtx: ExtensionContext | undefined;
	let requestRender: (() => void) | undefined;

	const installFooter = (ctx: ExtensionContext) => {
		if (ctx.mode !== "tui") return;
		ctx.ui.setFooter((tui, theme, footerData) => {
			requestRender = () => tui.requestRender();
			const unsubscribe = footerData.onBranchChange(requestRender);
			return {
				render: (width: number) => currentCtx
					? renderPowerline(width, stateFrom(currentCtx, footerData), theme)
					: [],
				invalidate: requestRender,
				dispose() {
					unsubscribe();
					requestRender = undefined;
				},
			};
		});
	};

	pi.on("session_start", (_event, ctx) => {
		currentCtx = ctx;
	});
	// Resource discovery runs after session_start. Install here so this appearance
	// wraps status providers that replace the footer during session startup.
	pi.on("resources_discover", (_event, ctx) => installFooter(ctx));

	const refresh = (_event: unknown, ctx: ExtensionContext) => {
		currentCtx = ctx;
		requestRender?.();
	};
	pi.on("model_select", refresh);
	pi.on("thinking_level_select", refresh);
	pi.on("session_info_changed", refresh);
	pi.on("session_compact", refresh);

	pi.on("session_shutdown", (_event, ctx) => {
		currentCtx = undefined;
		requestRender = undefined;
		if (ctx.mode === "tui") ctx.ui.setFooter(undefined);
	});
}
