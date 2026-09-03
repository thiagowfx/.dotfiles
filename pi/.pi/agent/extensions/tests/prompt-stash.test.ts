import assert from "node:assert/strict";
import test from "node:test";
import install from "../prompt-stash.ts";

type Handler = (event: unknown, ctx: any) => Promise<void> | void;
type Shortcut = { handler: (ctx: any) => Promise<void> | void };

test("session startup restores status without persisting unchanged state", async () => {
	const handlers = new Map<string, Handler>();
	const shortcuts = new Map<string, Shortcut>();
	const appended: Array<{ type: string; data: unknown }> = [];
	const statuses: Array<string | undefined> = [];
	let editorText = "draft";

	install({
		on(event: string, handler: Handler) {
			handlers.set(event, handler);
		},
		registerShortcut(key: string, shortcut: Shortcut) {
			shortcuts.set(key, shortcut);
		},
		registerCommand() {},
		appendEntry(type: string, data: unknown) {
			appended.push({ type, data });
		},
	} as never);

	const ctx = {
		mode: "tui",
		sessionManager: {
			getEntries: () => [
				{ type: "custom", customType: "prompt-stash", data: { stack: ["saved"] } },
			],
		},
		ui: {
			getEditorText: () => editorText,
			setEditorText: (text: string) => (editorText = text),
			setStatus: (_key: string, text: string | undefined) => statuses.push(text),
			notify() {},
		},
	};

	await handlers.get("session_start")?.({}, ctx);
	assert.deepEqual(appended, []);
	assert.equal(statuses.at(-1), "[⎇ 1 stashed]");

	await shortcuts.get("ctrl+s")?.handler(ctx);
	assert.deepEqual(appended, [
		{ type: "prompt-stash", data: { stack: ["saved", "draft"] } },
	]);
	assert.equal(editorText, "");
});
