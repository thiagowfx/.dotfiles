import { Type } from "typebox";
import { type ExtensionAPI, withFileMutationQueue } from "@earendil-works/pi-coding-agent";

import { appendMemory, buildMemorySnapshot, readMemory, resolveMemoryFile } from "./memory.ts";

export default function memoryWriteExtension(pi: ExtensionAPI) {
	const memoryFile = resolveMemoryFile();
	let memorySnapshot = "";

	pi.on("session_start", async () => {
		memorySnapshot = buildMemorySnapshot(await readMemory(memoryFile));
	});

	pi.on("before_agent_start", (event) => {
		if (!memorySnapshot) return;

		return {
			systemPrompt: `${event.systemPrompt}\n\n## Durable memory\n\n${memorySnapshot}`,
		};
	});

	pi.registerTool({
		name: "memory_write",
		label: "Memory Write",
		description: "Persist one user-approved durable fact or preference in MEMORY.md.",
		promptSnippet: "Persist user-approved durable facts and preferences",
		promptGuidelines: [
			'Use memory_write immediately when the user says "remember this" or explicitly asks to retain a durable fact.',
		],
		parameters: Type.Object({
			content: Type.String({ description: "Durable fact or preference to remember", minLength: 1 }),
		}),
		async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
			return withFileMutationQueue(memoryFile, async () => {
				const result = await appendMemory(memoryFile, params.content, ctx.sessionManager.getSessionId());
				return {
					content: [{ type: "text", text: result.written ? "Remembered." : "Already remembered." }],
					details: { path: memoryFile, written: result.written },
				};
			});
		},
	});
}
