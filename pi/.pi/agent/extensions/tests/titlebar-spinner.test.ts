import assert from "node:assert/strict";
import test from "node:test";
import install, { getBaseTitle } from "../titlebar-spinner.ts";

test("builds title from session context cwd", () => {
	assert.equal(getBaseTitle({ getSessionName: () => "Fix footer" }, "/repo/.worktrees/footer"), "π - Fix footer - footer");
	assert.equal(getBaseTitle({ getSessionName: () => undefined }, "/repo"), "π - repo");
});

test("updates idle title on session changes and settles only after all agent work", async () => {
	const handlers = new Map<string, (event: unknown, ctx: any) => Promise<void> | void>();
	let sessionName: string | undefined;
	const titles: string[] = [];
	install({
		on(event: string, handler: (event: unknown, ctx: any) => Promise<void> | void) {
			handlers.set(event, handler);
		},
		getSessionName: () => sessionName,
	} as never);
	const ctx = { cwd: "/repo", ui: { setTitle: (title: string) => titles.push(title) } };

	await handlers.get("session_start")?.({}, ctx);
	sessionName = "New name";
	await handlers.get("session_info_changed")?.({}, ctx);
	await handlers.get("agent_start")?.({}, ctx);
	assert.equal(handlers.has("agent_end"), false);
	await handlers.get("agent_settled")?.({}, ctx);

	assert.deepEqual(titles, ["π - repo", "π - New name - repo", "π - New name - repo", "π - New name - repo"]);
});
