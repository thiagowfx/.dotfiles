import assert from "node:assert/strict";
import { mkdtemp, readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";

import { appendMemory, buildMemorySnapshot, readMemory, resolveMemoryFile } from "./memory.ts";

test("appendMemory creates memory file and preserves hostile text", async () => {
	const directory = await mkdtemp(join(tmpdir(), "pi-memory-write-"));
	const memoryFile = join(directory, "nested", "MEMORY.md");
	const content = "#preference Keep `backticks`, $HOME, and an 'unbalanced quote.";

	const result = await appendMemory(memoryFile, content, "019ff01c-session", new Date("2026-08-11T12:00:00Z"));

	assert.equal(result.written, true);
	assert.equal(
		await readFile(memoryFile, "utf8"),
		"<!-- 2026-08-11T12:00:00.000Z [019ff01c] -->\n#preference Keep `backticks`, $HOME, and an 'unbalanced quote.\n",
	);
});

test("appendMemory skips an exact duplicate", async () => {
	const directory = await mkdtemp(join(tmpdir(), "pi-memory-write-"));
	const memoryFile = join(directory, "MEMORY.md");

	await appendMemory(memoryFile, "Remember me.", "first-session", new Date("2026-08-11T12:00:00Z"));
	const duplicate = await appendMemory(memoryFile, "  Remember me.  ", "second-session");

	assert.equal(duplicate.written, false);
	assert.equal((await readFile(memoryFile, "utf8")).match(/Remember me\./g)?.length, 1);
});

test("appendMemory rejects empty content", async () => {
	const directory = await mkdtemp(join(tmpdir(), "pi-memory-write-"));
	await assert.rejects(() => appendMemory(join(directory, "MEMORY.md"), "  ", "session"), /must not be empty/);
});

test("readMemory returns empty content for a missing file", async () => {
	assert.equal(await readMemory(join(tmpdir(), "missing-pi-memory-write-file")), "");
});

test("buildMemorySnapshot preserves both ends within limit", () => {
	const snapshot = buildMemorySnapshot(`START-${"x".repeat(100)}-END`, 50);

	assert.equal(snapshot.length, 50);
	assert.match(snapshot, /^START-/);
	assert.match(snapshot, /-END$/);
	assert.match(snapshot, /memory truncated/);
});

test("resolveMemoryFile honors isolated override", () => {
	assert.equal(resolveMemoryFile({ PI_MEMORY_FILE: "/tmp/custom-memory.md" }), "/tmp/custom-memory.md");
});
