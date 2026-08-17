import assert from "node:assert/strict";
import test from "node:test";
import install, { MCP_STATUS_EVENT } from "../mcp-startup.ts";

interface TestContext {
	mode: string;
	ui: {
		notify(message: string, type: string): void;
		theme: { fg(color: string, text: string): string };
	};
}

function createHarness() {
	const lifecycleHandlers = new Map<string, (event: unknown, ctx: TestContext) => void>();
	const eventHandlers = new Map<string, (data: unknown) => void>();
	const notifications: Array<{ message: string; type: string }> = [];
	const ctx: TestContext = {
		mode: "tui",
		ui: {
			notify(message, type) {
				notifications.push({ message, type });
			},
			theme: { fg: (color, text) => `${color}(${text})` },
		},
	};

	install({
		on(event: string, handler: (event: unknown, ctx: TestContext) => void) {
			lifecycleHandlers.set(event, handler);
		},
		events: {
			on(event: string, handler: (data: unknown) => void) {
				eventHandlers.set(event, handler);
			},
		},
	} as never);

	return { ctx, eventHandlers, lifecycleHandlers, notifications };
}

const snapshot = {
	version: 1,
	servers: [
		{ name: "slack-mcp", disabled: false },
		{ name: "disabled", disabled: true },
		{ name: "grafana", disabled: false },
	],
};

test("shows sorted enabled MCP servers after startup", () => {
	const harness = createHarness();
	harness.lifecycleHandlers.get("session_start")?.({}, harness.ctx);
	harness.eventHandlers.get(MCP_STATUS_EVENT)?.(snapshot);

	assert.deepEqual(harness.notifications, [{
		message: "mdHeading([MCP])\ndim(  grafana, slack-mcp)",
		type: "info",
	}]);
});

test("shows snapshot received before session_start", () => {
	const harness = createHarness();
	harness.eventHandlers.get(MCP_STATUS_EVENT)?.(snapshot);
	harness.lifecycleHandlers.get("session_start")?.({}, harness.ctx);

	assert.equal(harness.notifications.length, 1);
});

test("shows one block per session", () => {
	const harness = createHarness();
	harness.lifecycleHandlers.get("session_start")?.({}, harness.ctx);
	harness.eventHandlers.get(MCP_STATUS_EVENT)?.(snapshot);
	harness.eventHandlers.get(MCP_STATUS_EVENT)?.(snapshot);

	assert.equal(harness.notifications.length, 1);

	harness.lifecycleHandlers.get("session_shutdown")?.({}, harness.ctx);
	harness.lifecycleHandlers.get("session_start")?.({}, harness.ctx);

	assert.equal(harness.notifications.length, 2);
});

test("skips empty, invalid, and non-TUI snapshots", () => {
	const harness = createHarness();
	harness.lifecycleHandlers.get("session_start")?.({}, harness.ctx);
	harness.eventHandlers.get(MCP_STATUS_EVENT)?.({ version: 1, servers: [] });
	harness.eventHandlers.get(MCP_STATUS_EVENT)?.({ version: 1, servers: [null] });

	harness.ctx.mode = "print";
	harness.eventHandlers.get(MCP_STATUS_EVENT)?.(snapshot);

	assert.deepEqual(harness.notifications, []);
});
