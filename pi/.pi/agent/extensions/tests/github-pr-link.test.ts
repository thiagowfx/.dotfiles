import assert from "node:assert/strict";
import test from "node:test";
import install, { findWorktreeRoot, formatPrLink, osc8Link } from "../github-pr-link.ts";

type Handler = (event: unknown, ctx: unknown) => Promise<void> | void;

const STATUS_TEXT_KEY = "github-pr";

interface ExecCall {
	command: string;
	args: string[];
	cwd?: string;
}

function setup(exec: (command: string, args: string[], options?: { cwd?: string }) => unknown) {
	const handlers = new Map<string, Handler>();
	const calls: ExecCall[] = [];
	install({
		on(event: string, handler: Handler) {
			handlers.set(event, handler);
		},
		exec(command: string, args: string[], options?: { cwd?: string }) {
			calls.push({ command, args, cwd: options?.cwd });
			return exec(command, args, options);
		},
	} as never);
	return { handlers, calls };
}

function ctxWith(
	statuses: Array<{ key: string; text: string | undefined }>,
	signal?: AbortSignal,
	branch: unknown[] = [],
) {
	return {
		cwd: "/repo",
		signal,
		sessionManager: {
			getBranch() {
				return branch;
			},
		},
		ui: {
			setStatus(key: string, text: string | undefined) {
				statuses.push({ key, text });
			},
		},
	};
}

const OPEN_PR = JSON.stringify({
	number: 42,
	url: "https://github.com/o/r/pull/42",
	state: "OPEN",
});

test("formatPrLink links open PRs and hides everything else", () => {
	assert.equal(
		formatPrLink(OPEN_PR),
		"\x1b]8;;https://github.com/o/r/pull/42\x07PR #42\x1b]8;;\x07",
	);
	assert.equal(formatPrLink(JSON.stringify({ number: 42, state: "MERGED" })), undefined);
	assert.equal(formatPrLink(JSON.stringify({ number: 42, state: "CLOSED" })), undefined);
	assert.equal(formatPrLink(JSON.stringify({ state: "OPEN" })), undefined);
	assert.equal(formatPrLink("not json"), undefined);
	assert.equal(formatPrLink(JSON.stringify({ number: 7, state: "OPEN" })), "PR #7");
});

test("osc8Link rejects non-http schemes", () => {
	assert.equal(osc8Link("file:///etc/passwd", "PR #1"), "PR #1");
	assert.equal(osc8Link("nonsense", "PR #1"), "PR #1");
});

test("findWorktreeRoot finds the latest session worktree", () => {
	assert.equal(
		findWorktreeRoot(
			{
				oldPath: "/repo/.worktrees/old/file.ts",
				command: "cd /repo/.worktrees/current && gh pr view",
			},
			"/repo",
		),
		"/repo/.worktrees/current",
	);
	assert.equal(findWorktreeRoot("/other/.worktrees/topic/file.ts", "/repo"), undefined);
});

test("session start shows the PR link", async () => {
	const { handlers, calls } = setup(() => ({ stdout: OPEN_PR, stderr: "", code: 0, killed: false }));
	const statuses: Array<{ key: string; text: string | undefined }> = [];
	await handlers.get("session_start")?.({}, ctxWith(statuses));

	assert.deepEqual(calls, [
		{ command: "gh", args: ["pr", "view", "--json", "number,url,state"], cwd: "/repo" },
	]);
	assert.deepEqual(statuses, [
		{ key: STATUS_TEXT_KEY, text: "\x1b]8;;https://github.com/o/r/pull/42\x07PR #42\x1b]8;;\x07" },
	]);
});

test("turn end follows a worktree used by tools", async () => {
	const { handlers, calls } = setup(() => ({ stdout: OPEN_PR, stderr: "", code: 0, killed: false }));
	const statuses: Array<{ key: string; text: string | undefined }> = [];
	const ctx = ctxWith(statuses);

	await handlers.get("tool_call")?.(
		{ input: { path: "/repo/.worktrees/topic/src/file.ts" } },
		ctx,
	);
	await handlers.get("turn_end")?.({}, ctx);

	assert.deepEqual(calls, [
		{
			command: "gh",
			args: ["pr", "view", "--json", "number,url,state"],
			cwd: "/repo/.worktrees/topic",
		},
	]);
	assert.deepEqual(statuses, [
		{ key: STATUS_TEXT_KEY, text: "\x1b]8;;https://github.com/o/r/pull/42\x07PR #42\x1b]8;;\x07" },
	]);
});

test("session start restores the latest worktree from session history", async () => {
	const { handlers, calls } = setup(() => ({ stdout: OPEN_PR, stderr: "", code: 0, killed: false }));
	const statuses: Array<{ key: string; text: string | undefined }> = [];
	const branch = [
		{
			type: "message",
			message: {
				role: "assistant",
				content: [
					{
						type: "toolCall",
						arguments: { path: "/repo/.worktrees/restored/README.md" },
					},
				],
			},
		},
	];

	await handlers.get("session_start")?.({}, ctxWith(statuses, undefined, branch));

	assert.deepEqual(calls, [
		{
			command: "gh",
			args: ["pr", "view", "--json", "number,url,state"],
			cwd: "/repo/.worktrees/restored",
		},
	]);
});

test("turn end clears the entry when gh fails", async () => {
	const { handlers } = setup(() => ({
		stdout: "",
		stderr: "no pull requests found",
		code: 1,
		killed: false,
	}));
	const statuses: Array<{ key: string; text: string | undefined }> = [];
	await handlers.get("turn_end")?.({}, ctxWith(statuses));

	assert.deepEqual(statuses, [{ key: STATUS_TEXT_KEY, text: undefined }]);
});

test("turn end clears the entry when gh is missing", async () => {
	const { handlers } = setup(() => {
		throw new Error("spawn gh ENOENT");
	});
	const statuses: Array<{ key: string; text: string | undefined }> = [];
	await handlers.get("turn_end")?.({}, ctxWith(statuses));

	assert.deepEqual(statuses, [{ key: STATUS_TEXT_KEY, text: undefined }]);
});

test("aborted turn keeps the previous entry", async () => {
	const { handlers } = setup(() => {
		throw new Error("aborted");
	});
	const statuses: Array<{ key: string; text: string | undefined }> = [];
	const controller = new AbortController();
	controller.abort();
	await handlers.get("turn_end")?.({}, ctxWith(statuses, controller.signal));

	assert.deepEqual(statuses, []);
});

test("session shutdown clears the entry", async () => {
	const { handlers } = setup(() => ({ stdout: OPEN_PR, stderr: "", code: 0, killed: false }));
	const statuses: Array<{ key: string; text: string | undefined }> = [];
	await handlers.get("session_shutdown")?.({}, ctxWith(statuses));

	assert.deepEqual(statuses, [{ key: STATUS_TEXT_KEY, text: undefined }]);
});

test("a stale refresh does not overwrite a newer one", async () => {
	let resolveFirst: ((value: unknown) => void) | undefined;
	let call = 0;
	const { handlers } = setup(() => {
		call += 1;
		if (call === 1) return new Promise((resolve) => (resolveFirst = resolve));
		return { stdout: OPEN_PR, stderr: "", code: 0, killed: false };
	});
	const statuses: Array<{ key: string; text: string | undefined }> = [];
	const ctx = ctxWith(statuses);
	const stale = handlers.get("session_start")?.({}, ctx);
	await handlers.get("turn_end")?.({}, ctx);
	resolveFirst?.({ stdout: JSON.stringify({ number: 1, state: "OPEN" }), code: 0, killed: false });
	await stale;

	assert.deepEqual(statuses, [
		{ key: STATUS_TEXT_KEY, text: "\x1b]8;;https://github.com/o/r/pull/42\x07PR #42\x1b]8;;\x07" },
	]);
});
