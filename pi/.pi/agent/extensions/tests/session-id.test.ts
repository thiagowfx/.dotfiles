import assert from "node:assert/strict";
import test from "node:test";
import install from "../session-id.ts";

test("shows full session ID in footer", async () => {
	const handlers = new Map<string, (event: unknown, ctx: unknown) => void>();
	install({
		on(event: string, handler: (event: unknown, ctx: unknown) => void) {
			handlers.set(event, handler);
		},
	} as never);

	const statuses: Array<{ key: string; text: string }> = [];
	handlers.get("session_start")?.({}, {
		sessionManager: {
			getSessionId: () => "4f5fcdb8-6309-4178-a3de-a02177b14e33",
		},
		ui: {
			setStatus(key: string, text: string) {
				statuses.push({ key, text });
			},
			theme: { fg: (color: string, text: string) => `${color}(${text})` },
		},
	});

	assert.deepEqual(statuses, [{
		key: "session-id",
		text: "dim(sid:4f5fcdb8-6309-4178-a3de-a02177b14e33)",
	}]);
});
