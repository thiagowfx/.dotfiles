import { appendFile, mkdir, readFile } from "node:fs/promises";
import { homedir } from "node:os";
import { dirname, join, resolve } from "node:path";

export const MEMORY_SNAPSHOT_MAX_CHARS = 4_000;
const TRUNCATION_MARKER = "\n... (memory truncated) ...\n";

export function resolveMemoryFile(env: NodeJS.ProcessEnv = process.env): string {
	if (env.PI_MEMORY_FILE) return resolve(env.PI_MEMORY_FILE);
	return join(env.HOME ?? homedir(), ".pi", "agent", "memory", "MEMORY.md");
}

export async function readMemory(memoryFile: string): Promise<string> {
	try {
		return await readFile(memoryFile, "utf8");
	} catch (error) {
		if ((error as NodeJS.ErrnoException).code === "ENOENT") return "";
		throw error;
	}
}

export function buildMemorySnapshot(content: string, maxChars = MEMORY_SNAPSHOT_MAX_CHARS): string {
	const normalized = content.trim();
	if (!normalized || maxChars <= 0) return "";
	if (normalized.length <= maxChars) return normalized;

	const available = maxChars - TRUNCATION_MARKER.length;
	if (available <= 0) return normalized.slice(0, maxChars);

	const headChars = Math.ceil(available / 2);
	const tailChars = Math.floor(available / 2);
	return `${normalized.slice(0, headChars)}${TRUNCATION_MARKER}${normalized.slice(-tailChars)}`;
}

function rememberedEntries(content: string): string[] {
	return content
		.split(/^<!--[^\n]*-->\n/gm)
		.map((entry) => entry.trim())
		.filter(Boolean);
}

export interface AppendMemoryResult {
	written: boolean;
	entry: string;
}

export async function appendMemory(
	memoryFile: string,
	content: string,
	sessionId: string,
	now = new Date(),
): Promise<AppendMemoryResult> {
	const normalized = content.trim();
	if (!normalized) throw new Error("Memory content must not be empty.");

	const existing = await readMemory(memoryFile);
	if (rememberedEntries(existing).includes(normalized)) {
		return { written: false, entry: normalized };
	}

	await mkdir(dirname(memoryFile), { recursive: true });
	const shortSessionId = sessionId.slice(0, 8);
	const entry = `<!-- ${now.toISOString()} [${shortSessionId}] -->\n${normalized}`;
	const separator = existing.trim() ? "\n\n" : "";
	await appendFile(memoryFile, `${separator}${entry}\n`, "utf8");
	return { written: true, entry: normalized };
}
