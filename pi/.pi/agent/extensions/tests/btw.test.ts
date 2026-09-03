import assert from "node:assert/strict";
import test from "node:test";
import { contextMessagesFrom } from "../lib/btw-context.ts";

test("contextMessagesFrom projects compaction-aware entries", () => {
	const source = {
		buildContextEntries: () => [
			{ type: "compaction", summary: "Earlier work" },
			{ type: "message", text: "Recent request" },
			{ type: "custom", data: {} },
		],
	};

	const messages = contextMessagesFrom(source, (entry) => {
		if (entry.type === "compaction") return [`summary: ${entry.summary}`];
		if (entry.type === "message") return [entry.text!];
		return [];
	});

	assert.deepEqual(messages, ["summary: Earlier work", "Recent request"]);
});
