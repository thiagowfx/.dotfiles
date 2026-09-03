import assert from "node:assert/strict";
import test from "node:test";
import install, {
	collectUsage,
	formatTokens,
	hasNerdFonts,
	renderPowerline,
	visibleWidth,
	type PowerlineState,
} from "../powerline-status.ts";

const theme = {
	fg(_color: string, text: string) {
		return text;
	},
};

function state(overrides: Partial<PowerlineState> = {}): PowerlineState {
	return {
		modelName: "Claude Opus 5",
		modelReasoning: true,
		thinkingLevel: "high",
		cwd: "/repo",
		branch: "main",
		usage: { input: 0, output: 0, cacheRead: 12_000, cacheWrite: 0, cost: 1.012 },
		context: { tokens: 50_000, contextWindow: 200_000, percent: 25 },
		usingSubscription: true,
		statuses: new Map([
			["session-id", "sid:123"],
			["github-pr", "PR #42"],
		]),
		nerdFonts: false,
		...overrides,
	};
}

function plain(value: string): string {
	return value.replace(/\x1b\[[0-?]*[ -/]*[@-~]/g, "");
}

test("renders powerline content in package order", () => {
	const lines = renderPowerline(200, state(), theme);
	assert.deepEqual(lines.map(plain), [
		" Opus 5 | think:high | dir repo | ⎇ main | ◫ 50k/200k (25.0%) | cache in: 12k | $1.012 (sub) | PR #42 · sid:123 ",
	]);
});

test("moves whole segments to second row and respects width", () => {
	const lines = renderPowerline(28, state({ statuses: new Map() }), theme);
	assert.equal(lines.length, 2);
	assert.ok(lines.every((line) => visibleWidth(line) <= 28));
	assert.match(plain(lines[0] ?? ""), /Opus 5/);
	assert.match(plain(lines.join("\n")), /think:high/);
	assert.match(plain(lines[1] ?? ""), /dir repo/);
});

test("uses warning and error context colors at thresholds", () => {
	const colors: string[] = [];
	const recordingTheme = {
		fg(color: string, text: string) {
			colors.push(color);
			return text;
		},
	};
	renderPowerline(200, state({ context: { tokens: 150, contextWindow: 200, percent: 75 } }), recordingTheme);
	assert.ok(colors.includes("warning"));
	colors.length = 0;
	renderPowerline(200, state({ context: { tokens: 190, contextWindow: 200, percent: 95 } }), recordingTheme);
	assert.ok(colors.includes("error"));
});

test("collects billed usage from assistant, tool, compaction, and summary entries", () => {
	const usage = (input: number, cost: number) => ({
		input,
		output: input + 1,
		cacheRead: input + 2,
		cacheWrite: input + 3,
		cost: { total: cost },
	});
	assert.deepEqual(collectUsage([
		{ type: "message", message: { role: "assistant", usage: usage(1, 0.1) } },
		{ type: "message", message: { role: "toolResult", usage: usage(2, 0.2) } },
		{ type: "message", message: { role: "user", usage: usage(99, 99) } },
		{ type: "compaction", usage: usage(3, 0.3) },
		{ type: "branch_summary", usage: usage(4, 0.4) },
		{ type: "compaction", usage: "bad" },
	]), {
		input: 10,
		output: 14,
		cacheRead: 18,
		cacheWrite: 22,
		cost: 1,
	});
});

test("measures Unicode terminal width", () => {
	assert.equal(visibleWidth("界"), 2);
	assert.equal(visibleWidth("🚀"), 2);
	assert.equal(visibleWidth("e\u0301"), 1);
	assert.equal(visibleWidth("\x1b[31m界\x1b[0m"), 2);
});

test("formats tokens and detects Nerd Font terminals", () => {
	assert.equal(formatTokens(999), "999");
	assert.equal(formatTokens(1_250), "1.3k");
	assert.equal(formatTokens(15_000), "15k");
	assert.equal(formatTokens(1_250_000), "1.3M");
	assert.equal(hasNerdFonts({ POWERLINE_NERD_FONTS: "1" }), true);
	assert.equal(hasNerdFonts({ POWERLINE_NERD_FONTS: "0", TERM_PROGRAM: "Ghostty" }), false);
	assert.equal(hasNerdFonts({ TERM_PROGRAM: "WezTerm" }), true);
});

test("installs only custom footer behavior in TUI mode", () => {
	const handlers = new Map<string, (event: unknown, ctx: any) => void>();
	install({
		on(event: string, handler: (event: unknown, ctx: any) => void) {
			handlers.set(event, handler);
		},
	} as never);

	let footerFactory: ((tui: any, theme: any, data: any) => any) | undefined;
	const ctx = {
		mode: "tui",
		cwd: "/repo",
		model: { id: "model", name: "Model", provider: "test", reasoning: false },
		thinkingLevel: "off",
		modelRegistry: { isUsingOAuth: () => false },
		sessionManager: { getEntries: () => [] },
		getContextUsage: () => undefined,
		ui: { setFooter: (factory: typeof footerFactory) => (footerFactory = factory) },
	};
	handlers.get("session_start")?.({}, ctx);
	assert.equal(footerFactory, undefined);
	handlers.get("resources_discover")?.({}, ctx);
	assert.ok(footerFactory);

	let renders = 0;
	let unsubscribed = false;
	const component = footerFactory?.(
		{ requestRender: () => (renders += 1) },
		theme,
		{
			getGitBranch: () => null,
			getExtensionStatuses: () => new Map(),
			onBranchChange: (callback: () => void) => {
				callback();
				return () => (unsubscribed = true);
			},
		},
	);
	const lines = component.render(80).map(plain);
	assert.equal(lines.length, 1);
	assert.match(lines[0] ?? "", /Model/);
	assert.match(lines[0] ?? "", /repo/);
	assert.equal(renders, 1);
	component.dispose();
	assert.equal(unsubscribed, true);
});
