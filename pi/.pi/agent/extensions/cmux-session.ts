// cmux-pi-session-extension-marker v2
// Bridges Pi session lifecycle, tool telemetry, notifications, and resume bindings into cmux.
// Installed by `cmux hooks pi install` or `cmux hooks setup`.
// DO NOT EDIT MANUALLY. cmux upgrades this file in place.

import { Buffer } from "node:buffer";
import { spawn } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

type HookExtra = Record<string, unknown>;

interface PendingCompletion {
  lastAssistantMessage?: string;
  notificationType: string;
  turnId: string;
}

interface SessionState {
  nextTurn: number;
  activeTurnId?: string;
  pendingCompletion?: PendingCompletion;
  feedDeliveryFailed: boolean;
  stopped: boolean;
}

interface CommandResult {
  ok: boolean;
  status: number | null;
  stdout: string;
  stderr: string;
  error?: unknown;
  surfaceUnavailable?: boolean;
}

interface PiExtensionContextSnapshot {
  readonly sessionId: string | null;
  readonly cwd: string;
  readonly notifyWarning?: () => void;
}

function firstString(...values: unknown[]): string | null {
  for (const value of values) {
    if (typeof value === "string" && value.trim().length > 0) return value.trim();
  }
  return null;
}

function objectValue(value: unknown, keys: string[]): unknown {
  if (!value || typeof value !== "object") return undefined;
  const typed = value as Record<string, unknown>;
  for (const key of keys) {
    if (typed[key] !== undefined && typed[key] !== null) return typed[key];
  }
  return undefined;
}

function utf8Prefix(value: unknown, maximumBytes: number): string | undefined {
  if (typeof value !== "string") return undefined;
  const candidate = value.length > maximumBytes ? value.slice(0, maximumBytes) : value;
  const bytes = Buffer.from(candidate, "utf8");
  if (bytes.byteLength <= maximumBytes) return candidate;
  return bytes.subarray(0, maximumBytes).toString("utf8").replace(/\uFFFD+$/u, "");
}

interface PiFeedProjectionState {
  remainingNodes: number;
  seen: WeakSet<object>;
}

function projectPiFeedValue(value: unknown, state: PiFeedProjectionState, depth = 0, preserveText = true): unknown {
  if (value === null || typeof value === "boolean") return value;
  if (typeof value === "string") return preserveText ? utf8Prefix(value, 512) : piFeedValueSummary(value);
  if (typeof value === "number") {
    return preserveText && Number.isFinite(value) ? value : piFeedValueSummary(value);
  }
  if (typeof value !== "object") return piFeedValueSummary(value);
  if (depth >= 4 || state.remainingNodes <= 0) return piFeedValueSummary(value);
  if (state.seen.has(value)) return { kind: "circular" };
  state.remainingNodes -= 1;
  state.seen.add(value);
  try {
    if (Array.isArray(value)) {
      const out: unknown[] = [];
      const retained = Math.min(value.length, 12);
      for (let index = 0; index < retained; index += 1) {
        try {
          out.push(projectPiFeedValue(value[index], state, depth + 1, preserveText));
        } catch (_) {
          out.push({ kind: "unavailable" });
        }
      }
      if (value.length > retained) out.push({ kind: "omitted", count: value.length - retained });
      return out;
    }
    const out: Record<string, unknown> = {};
    let scanned = 0;
    try {
      for (const key in value as Record<string, unknown>) {
        if (scanned >= 12) {
          out.cmux_truncated = true;
          break;
        }
        scanned += 1;
        if (!Object.prototype.hasOwnProperty.call(value, key)) continue;
        const projectedKey = utf8Prefix(key, 128);
        if (!projectedKey) continue;
        try {
          out[projectedKey] = projectPiFeedValue(
            (value as Record<string, unknown>)[key],
            state,
            depth + 1,
            preserveText,
          );
        } catch (_) {
          out[projectedKey] = { kind: "unavailable" };
        }
      }
    } catch (_) {
      return piFeedValueSummary(value);
    }
    return out;
  } finally {
    state.seen.delete(value);
  }
}

function boundedPiFeedInput(payload: Record<string, unknown>, maximumBytes: number): string {
  const serialized = JSON.stringify(payload);
  if (Buffer.byteLength(serialized, "utf8") <= maximumBytes) return serialized;

  const summaries = Array.isArray(payload.cmux_compacted_terminal_events)
    ? payload.cmux_compacted_terminal_events
    : [];
  const latest = summaries.length > 0 && typeof summaries[summaries.length - 1] === "object"
    ? summaries[summaries.length - 1] as Record<string, unknown>
    : undefined;
  const rawCount = payload.cmux_compacted_terminal_count;
  const totalCount = typeof rawCount === "number" && Number.isFinite(rawCount)
    ? Math.max(summaries.length, rawCount)
    : summaries.length;
  const rawOmitted = payload.cmux_compacted_terminal_omitted_count;
  const omittedCount = typeof rawOmitted === "number" && Number.isFinite(rawOmitted)
    ? Math.max(0, rawOmitted, totalCount - 1)
    : Math.max(0, totalCount - 1);
  const safe: Record<string, unknown> = {};
  for (const key of ["session_id", "cwd", "turn_id", "tool_call_id", "tool_name"] as const) {
    const value = utf8Prefix(payload[key], 256);
    if (value !== undefined) safe[key] = value;
  }
  for (const key of ["hook_event_name", "event"] as const) {
    const value = utf8Prefix(payload[key], 64);
    if (value !== undefined) safe[key] = value;
  }
  if (typeof payload.is_error === "boolean") safe.is_error = payload.is_error;
  if (latest) {
    const summary: Record<string, unknown> = {};
    for (const key of ["session_id", "cwd", "turn_id", "tool_call_id", "tool_name"] as const) {
      const value = utf8Prefix(latest[key] ?? payload[key], 256);
      if (value !== undefined) summary[key] = value;
    }
    if (typeof latest.is_error === "boolean") summary.is_error = latest.is_error;
    safe.cmux_compacted_terminal_count = totalCount;
    safe.cmux_compacted_terminal_omitted_count = omittedCount;
    safe.cmux_compacted_terminal_events = [summary];
  } else if (firstString(payload.hook_event_name, payload.event) === "PostToolUse") {
    safe.cmux_compacted_terminal_count = 1;
    safe.cmux_compacted_terminal_omitted_count = 0;
    safe.cmux_compacted_terminal_events = [piTerminalFeedSummary(payload)];
  } else if (payload.tool_input !== undefined) {
    safe.tool_input = piFeedValueSummary(payload.tool_input);
  }

  const compacted = JSON.stringify(safe);
  if (Buffer.byteLength(compacted, "utf8") <= maximumBytes) return compacted;
  const fallbackEvent = utf8Prefix(payload.hook_event_name, 64) || "PostToolUse";
  return JSON.stringify({
    session_id: utf8Prefix(payload.session_id, 128),
    hook_event_name: fallbackEvent,
    event: fallbackEvent,
    tool_call_id: "compacted-overflow",
    tool_name: "cmux_compacted_terminal_overflow",
    tool_input: fallbackEvent === "PostToolUse"
      ? { omitted_terminal_count: Math.max(1, totalCount) }
      : piFeedValueSummary(payload.tool_input),
  });
}

function resolveExecutable(name: string): string {
  const pathEnv = process.env.PATH || "";
  for (const dir of pathEnv.split(path.delimiter)) {
    if (!dir) continue;
    const candidate = path.join(dir, name);
    try {
      fs.accessSync(candidate, fs.constants.X_OK);
      if (fs.statSync(candidate).isFile()) return candidate;
    } catch (_) {}
  }
  return name;
}

function looksLikePiExecutable(value: string): boolean {
  const base = path.basename(value).toLowerCase();
  return base === "pi" || base === "pi-coding-agent";
}

function looksLikePiScript(value: string): boolean {
  const normalized = value.replaceAll("\\", "/").toLowerCase();
  const base = path.basename(normalized);
  return (
    normalized.includes("/@earendil-works/pi-coding-agent/") ||
    normalized.includes("/@mariozechner/pi-coding-agent/") ||
    normalized.includes("/packages/coding-agent/") ||
    ((base === "cli.js" || base === "cli.ts") &&
      (normalized.includes("pi-coding-agent") || normalized.includes("coding-agent")))
  );
}

function normalizedLaunchArgv(): string[] {
  const raw = Array.isArray(process.argv) ? process.argv.map((value) => String(value)) : [];
  if (raw.length === 0) return [resolveExecutable("pi")];
  if (looksLikePiExecutable(raw[0])) return raw;
  if (raw.length > 1 && looksLikePiScript(raw[1])) {
    return [resolveExecutable("pi"), ...raw.slice(2)];
  }
  return [resolveExecutable("pi"), ...raw.slice(1)];
}

function detectedPiVersion(): string | null {
  const script = process.argv.slice(0, 2).find((value) => {
    const candidate = String(value);
    return looksLikePiScript(candidate) || looksLikePiExecutable(candidate);
  });
  if (!script) return null;
  let scriptPath = path.resolve(String(script));
  try {
    // npm launches through bin symlinks, so inspect the package containing the resolved script.
    scriptPath = fs.realpathSync(scriptPath);
  } catch (_) {}
  let directory = path.dirname(scriptPath);
  for (let depth = 0; depth < 8; depth += 1) {
    try {
      const packageJSON = JSON.parse(fs.readFileSync(path.join(directory, "package.json"), "utf8"));
      if (
        packageJSON?.name === "@earendil-works/pi-coding-agent" ||
        packageJSON?.name === "@mariozechner/pi-coding-agent"
      ) {
        return firstString(packageJSON.version);
      }
    } catch (_) {}
    const parent = path.dirname(directory);
    if (parent === directory) break;
    directory = parent;
  }
  return null;
}

function supportsAgentSettled(): boolean {
  const version = detectedPiVersion();
  if (!version) return false;
  const match = /^(\d+)\.(\d+)\.(\d+)/.exec(version);
  if (!match) return false;
  const major = Number(match[1]);
  const minor = Number(match[2]);
  const patch = Number(match[3]);
  return major > 0 || minor > 80 || (minor === 80 && patch >= 5);
}

function base64NulSeparated(values: string[]): string {
  const bytes: Buffer[] = [];
  for (const value of values) {
    bytes.push(Buffer.from(String(value), "utf8"));
    bytes.push(Buffer.from([0]));
  }
  return Buffer.concat(bytes).toString("base64");
}

function secretLikeEnvKey(key: string): boolean {
  return /(TOKEN|SECRET|PASSWORD|PASSWD|API[_-]?KEY|ACCESS[_-]?KEY|PRIVATE[_-]?KEY|CREDENTIAL|AUTHORIZATION|COOKIE)/i.test(key);
}

function safePiEnvKey(key: string): boolean {
  return (
    key === "PI_CODING_AGENT_DIR" ||
    key === "PI_CONFIG_DIR" ||
    key === "PI_CODING_AGENT_SESSION_DIR" ||
    (key.startsWith("PI_CODING_AGENT_") && !secretLikeEnvKey(key))
  );
}

function safeNodeEnvKey(key: string): boolean {
  return (
    key === "NODE_ENV" ||
    key === "NODE_OPTIONS" ||
    key === "NODE_PATH" ||
    key === "NODE_NO_WARNINGS" ||
    key === "NODE_EXTRA_CA_CERTS"
  );
}

function safeCmuxEnvKey(key: string): boolean {
  if (key.startsWith("CMUX_TEST_PI_")) return !secretLikeEnvKey(key);
  if (key.startsWith("CMUX_AGENT_LAUNCH_")) return !secretLikeEnvKey(key);
  if (key === "CMUX_AGENT_HOOK_STATE_DIR") return true;
  if (key === "CMUX_PI_CMUX_BIN" || key === "CMUX_PI_HOOKS_DISABLED") return true;
  if (key === "CMUX_SURFACE_ID" || key === "CMUX_WORKSPACE_ID" || key === "CMUX_WINDOW_ID") return true;
  if (key === "CMUX_PANE_ID" || key === "CMUX_TAB_ID" || key === "CMUX_PANEL_ID") return true;
  if (key === "CMUX_SOCKET" || key === "CMUX_SOCKET_PATH") return true;
  if (key === "CMUX_BUNDLE_ID" || key === "CMUX_BUNDLED_CLI_PATH") return true;
  if (key === "CMUX_CLI_SENTRY_DISABLED" || key === "CMUX_DEBUG_LOG") return true;
  return false;
}

function shouldPreserveEnvKey(key: string): boolean {
  if (safeCmuxEnvKey(key)) return true;
  if (safePiEnvKey(key)) return true;
  if (safeNodeEnvKey(key)) return true;
  if (key === "PATH" || key === "HOME" || key === "PWD" || key === "SHELL") return true;
  if (key === "USER" || key === "LOGNAME" || key === "TMPDIR" || key === "TZ") return true;
  if (key === "LANG" || key.startsWith("LC_")) return true;
  if (key === "TERM" || key === "TERM_PROGRAM" || key === "TERM_PROGRAM_VERSION" || key === "COLORTERM") return true;
  if (key === "SSH_AUTH_SOCK") return true;
  if (key.startsWith("PI_") || key.startsWith("NODE_")) return !secretLikeEnvKey(key);
  return false;
}

function hookEnvironment(cwd: string, includeSocketPassword = false): NodeJS.ProcessEnv {
  const env: NodeJS.ProcessEnv = {};
  for (const [key, value] of Object.entries(process.env)) {
    if (value === undefined) continue;
    if (shouldPreserveEnvKey(key)) env[key] = value;
  }
  // Only cmux CLI children need the socket credential; keep it out of the generic allowlist.
  if (includeSocketPassword) {
    const socketPassword = process.env.CMUX_SOCKET_PASSWORD;
    if (socketPassword) env.CMUX_SOCKET_PASSWORD = socketPassword;
  }
  if (!env.CMUX_AGENT_LAUNCH_ARGV_B64) {
    const argv = normalizedLaunchArgv();
    env.CMUX_AGENT_LAUNCH_KIND = "pi";
    env.CMUX_AGENT_LAUNCH_EXECUTABLE = argv[0] || resolveExecutable("pi");
    env.CMUX_AGENT_LAUNCH_ARGV_B64 = base64NulSeparated(argv);
    env.CMUX_AGENT_LAUNCH_CWD = cwd || process.cwd();
  }
  return env;
}

function eventName(subcommand: string): string {
  switch (subcommand) {
    case "session-start":
      return "SessionStart";
    case "prompt-submit":
      return "UserPromptSubmit";
    case "stop":
      return "Stop";
    case "notification":
      return "Notification";
    default:
      return subcommand;
  }
}

function textFromContent(content: unknown): string | null {
  if (typeof content === "string") return content;
  if (!Array.isArray(content)) return null;
  const parts: string[] = [];
  for (const block of content) {
    if (!block || typeof block !== "object") continue;
    const typed = block as { type?: unknown; text?: unknown };
    if (typed.type === "text" && typeof typed.text === "string") parts.push(typed.text);
  }
  return parts.join("\n") || null;
}

function lastAssistantMessage(event: unknown): string | undefined {
  const messagesValue = objectValue(event, ["messages"]);
  const messages = Array.isArray(messagesValue) ? messagesValue : [];
  for (let index = messages.length - 1; index >= 0; index -= 1) {
    const message = messages[index];
    if (!message || typeof message !== "object") continue;
    const typed = message as { role?: unknown; content?: unknown };
    if (typed.role !== "assistant") continue;
    const text = firstString(textFromContent(typed.content));
    if (text) return text;
  }
  return undefined;
}

function sessionIdFrom(ctx: ExtensionContext): string | null {
  return firstString(ctx.sessionManager.getSessionId());
}

function cwdFrom(ctx: ExtensionContext): string {
  return firstString(ctx.cwd, process.cwd()) || process.cwd();
}

function snapshotContext(ctx: ExtensionContext): PiExtensionContextSnapshot {
  let notifyWarning: (() => void) | undefined;
  try {
    const ui = (ctx as unknown as { ui?: { notify?: (message: string, type?: string) => void } }).ui;
    if (typeof ui?.notify === "function") {
      notifyWarning = () => ui.notify?.("cmux Pi integration warning - check the terminal for details", "warning");
    }
  } catch (_) {}
  return {
    sessionId: sessionIdFrom(ctx),
    cwd: cwdFrom(ctx),
    notifyWarning,
  };
}

function stateFor(sessionStates: Map<string, SessionState>, sessionId: string): SessionState {
  let state = sessionStates.get(sessionId);
  if (!state) {
    state = { nextTurn: 0, feedDeliveryFailed: false, stopped: false };
    sessionStates.set(sessionId, state);
  }
  return state;
}

function eventTurnId(event: unknown): string | null {
  return firstString(
    objectValue(event, ["turn_id", "turnId", "turnID"])
  );
}

function beginTurn(sessionStates: Map<string, SessionState>, sessionId: string, event: unknown): string {
  const state = stateFor(sessionStates, sessionId);
  const turnId = eventTurnId(event) || `${sessionId}:turn-${state.nextTurn + 1}`;
  if (!eventTurnId(event)) state.nextTurn += 1;
  state.activeTurnId = turnId;
  state.pendingCompletion = undefined;
  state.stopped = false;
  return turnId;
}

function currentTurnId(sessionStates: Map<string, SessionState>, sessionId: string, event: unknown): string {
  const state = stateFor(sessionStates, sessionId);
  const turnId = eventTurnId(event) || state.activeTurnId || `${sessionId}:turn-${state.nextTurn + 1}`;
  if (!eventTurnId(event) && !state.activeTurnId) state.nextTurn += 1;
  return turnId;
}

function finishTurn(sessionStates: Map<string, SessionState>, sessionId: string, event: unknown): string {
  const state = stateFor(sessionStates, sessionId);
  const turnId = eventTurnId(event) || state.activeTurnId || `${sessionId}:turn-${state.nextTurn + 1}`;
  if (!eventTurnId(event) && !state.activeTurnId) state.nextTurn += 1;
  state.activeTurnId = undefined;
  state.pendingCompletion = undefined;
  state.stopped = true;
  return turnId;
}

function settleTurn(sessionStates: Map<string, SessionState>, sessionId: string): PendingCompletion | undefined {
  const state = sessionStates.get(sessionId);
  const completion = state?.pendingCompletion;
  if (!state || !completion || state.stopped) return undefined;
  state.activeTurnId = undefined;
  state.pendingCompletion = undefined;
  // Keep the settlement claim while awaiting delivery so session_shutdown cannot
  // emit a second Stop when terminal-feed delivery degrades.
  state.stopped = true;
  return completion;
}

function warn(
  ctx: PiExtensionContextSnapshot | null,
  message: string,
  details: Record<string, unknown> = {},
  notifyUser = false,
): void {
  const payload = { source: "cmux-pi-extension", level: "warning", message, ...details };
  try {
    console.warn(JSON.stringify(payload));
  } catch (_) {
    console.warn(`[cmux-pi-extension] ${message}`);
  }
  // Hook transport is best-effort telemetry. Keep routine command failures in
  // the terminal instead of interrupting Pi with a generic toast; reserve the
  // UI warning for an unexpected extension-task exception.
  if (notifyUser) {
    try {
      ctx?.notifyWarning?.();
    } catch (_) {}
  }
}

function cmuxExecutable(): string {
  return process.env.CMUX_PI_CMUX_BIN || "cmux";
}

interface PiFeedCommand {
  readonly args: string[];
  readonly cwd: string;
  readonly payload: Record<string, unknown>;
  readonly context: PiExtensionContextSnapshot;
  readonly terminal: boolean;
  readonly onFailure?: () => void;
}

interface PiCommandCancellation {
  cancelled: boolean;
  cancel?: () => void;
}
function piFeedValueSummary(value: unknown): Record<string, unknown> {
  if (value === null) return { kind: "null" };
  if (typeof value === "string") return { kind: "text", length: value.length };
  if (typeof value === "boolean" || typeof value === "number") return { kind: typeof value };
  if (Array.isArray(value)) return { kind: "array" };
  return { kind: typeof value };
}

function piTerminalFeedSummary(payload: Record<string, unknown>): Record<string, unknown> {
  const summary: Record<string, unknown> = {};
  for (const key of ["session_id", "turn_id", "tool_call_id", "tool_name", "cwd"] as const) {
    const value = payload[key];
    if (typeof value === "string") summary[key] = value.slice(0, 2048);
  }
  if (typeof payload.is_error === "boolean") summary.is_error = payload.is_error;
  if (payload.tool_result !== undefined) summary.tool_result = piFeedValueSummary(payload.tool_result);
  return summary;
}

class PiCmuxCommandDispatcher {
  private static readonly surfaceUnavailableExitCode = 69;
  private static readonly maxPendingFeedCommands = 8;
  private static readonly maxQueuedFeedCommands = 32;
  private static readonly maxActiveFeedCommands = 2;
  private static readonly maxCompactedTerminalSummaries = 64;
  // Leave headroom for the feed.push envelope under the relay's 16 KiB frame limit.
  private static readonly maxFeedInputBytes = 12 * 1024;
  // The app may spend three seconds committing acknowledged Feed ingress and the
  // CLI owns a four-second end-to-end deadline. Observe that outcome before the
  // extension classifies a terminal delivery as failed.
  private static readonly feedDrainDeadlineMs = 4500;
  private controlQueues = new Map<string | null, Promise<void>>();
  private pendingFeedCommands = new Map<string, PiFeedCommand>();
  private pendingFeedKeysBySession = new Map<string | null, string[]>();
  private priorityFeedCommands = new Map<string | null, PiFeedCommand[]>();
  private feedDrainWaiters = new Map<string, Array<() => void>>();
  private feedSessionQueue: Array<string | null> = [];
  private scheduledFeedSessions = new Set<string | null>();
  private unavailableSessions = new Set<string>();
  private activeFeeds = new Map<string | null, {
    cancellation: PiCommandCancellation;
    command: PiFeedCommand;
  }>();
  canDispatch(sessionId: string | null): boolean {
    return !sessionId || !this.unavailableSessions.has(sessionId);
  }
  releaseSession(sessionId: string): void {
    this.unavailableSessions.delete(sessionId);
  }

  run(
    args: string[],
    cwd: string,
    input: string | undefined,
    context: PiExtensionContextSnapshot,
  ): Promise<CommandResult> {
    const sessionId = context.sessionId;
    const previous = this.controlQueues.get(sessionId) || Promise.resolve();
    const scheduled = previous.then(() => this.execute(args, cwd, input, context));
    let tail: Promise<void>;
    tail = scheduled
      .then(() => undefined, () => undefined)
      .finally(() => {
        if (this.controlQueues.get(sessionId) === tail) this.controlQueues.delete(sessionId);
      });
    this.controlQueues.set(sessionId, tail);
    return scheduled;
  }
  enqueueFeed(key: string, command: PiFeedCommand): void {
    const sessionId = command.context.sessionId;
    if (!this.canDispatch(sessionId)) {
      if (command.terminal) command.onFailure?.();
      return;
    }
    const existing = this.pendingFeedCommands.get(key);
    if (existing) {
      // Once a completion is pending for a tool, never replace it with a late start event.
      if (existing.terminal && !command.terminal) return;
    } else {
      if (this.queuedFeedCount(sessionId) >= PiCmuxCommandDispatcher.maxPendingFeedCommands) {
        if (!command.terminal) return;
        if (!this.evictPendingStartForCompletion(sessionId)) {
          if (!this.compactPendingCompletion(command)) command.onFailure?.();
          return;
        }
      }
      if (this.totalQueuedFeedCount() >= PiCmuxCommandDispatcher.maxQueuedFeedCommands) {
        if (!command.terminal) return;
        if (!this.evictAnyPendingStart()) {
          if (!this.compactPendingCompletion(command)) command.onFailure?.();
          return;
        }
      }
    }
    // Reinsert coalesced entries so per-session order reflects event arrival.
    this.removePendingFeed(key);
    this.appendPendingFeed(key, command);
    this.scheduleFeed(sessionId);
  }
  async finishFeedForSession(sessionId: string): Promise<void> {
    for (const key of [...(this.pendingFeedKeysBySession.get(sessionId) || [])]) {
      const command = this.removePendingFeed(key);
      if (command?.terminal) this.appendPriorityFeed(command);
    }
    const active = this.activeFeeds.get(sessionId);
    if (active && !active.command.terminal) {
      active.cancellation.cancelled = true;
      active.cancellation.cancel?.();
    }
    this.scheduleFeed(sessionId);
    await this.waitForFeedDrainUntilDeadline(sessionId);
  }
  private queuedFeedCount(sessionId: string | null): number {
    return (this.pendingFeedKeysBySession.get(sessionId)?.length || 0)
      + (this.priorityFeedCommands.get(sessionId)?.length || 0);
  }

  private totalQueuedFeedCount(): number {
    let count = this.pendingFeedCommands.size;
    for (const commands of this.priorityFeedCommands.values()) count += commands.length;
    return count;
  }
  private appendPendingFeed(key: string, command: PiFeedCommand): void {
    const sessionId = command.context.sessionId;
    const keys = this.pendingFeedKeysBySession.get(sessionId) || [];
    keys.push(key);
    this.pendingFeedKeysBySession.set(sessionId, keys);
    this.pendingFeedCommands.set(key, command);
  }

  private removePendingFeed(key: string): PiFeedCommand | undefined {
    const command = this.pendingFeedCommands.get(key);
    if (!command) return undefined;
    this.pendingFeedCommands.delete(key);
    const sessionId = command.context.sessionId;
    const keys = this.pendingFeedKeysBySession.get(sessionId) || [];
    const index = keys.indexOf(key);
    if (index >= 0) keys.splice(index, 1);
    if (keys.length) this.pendingFeedKeysBySession.set(sessionId, keys);
    else this.pendingFeedKeysBySession.delete(sessionId);
    return command;
  }

  private appendPriorityFeed(command: PiFeedCommand): void {
    const sessionId = command.context.sessionId;
    const commands = this.priorityFeedCommands.get(sessionId) || [];
    commands.push(command);
    this.priorityFeedCommands.set(sessionId, commands);
  }

  private takeNextFeed(sessionId: string | null): PiFeedCommand | undefined {
    const priority = this.priorityFeedCommands.get(sessionId);
    const command = priority?.shift();
    if (priority && !priority.length) this.priorityFeedCommands.delete(sessionId);
    if (command) return command;
    const key = this.pendingFeedKeysBySession.get(sessionId)?.[0];
    return key === undefined ? undefined : this.removePendingFeed(key);
  }

  private waitForFeedDrain(sessionId: string): Promise<void> {
    if (!this.hasFeedWork(sessionId)) return Promise.resolve();
    return new Promise<void>((resolve) => {
      const waiters = this.feedDrainWaiters.get(sessionId) || [];
      waiters.push(resolve);
      this.feedDrainWaiters.set(sessionId, waiters);
    });
  }

  private waitForFeedDrainUntilDeadline(sessionId: string): Promise<void> {
    if (!this.hasFeedWork(sessionId)) return Promise.resolve();
    const drained = this.waitForFeedDrain(sessionId);
    return new Promise<void>((resolve) => {
      let settled = false;
      const finish = () => {
        if (settled) return;
        settled = true;
        clearTimeout(deadline);
        resolve();
      };
      const deadline = setTimeout(() => {
        this.failTerminalFeedForSession(sessionId);
        this.discardFeedForSession(sessionId);
        finish();
      }, PiCmuxCommandDispatcher.feedDrainDeadlineMs);
      void drained.then(finish);
    });
  }

  private hasFeedWork(sessionId: string): boolean {
    return this.activeFeeds.has(sessionId) || this.queuedFeedCount(sessionId) > 0;
  }

  private resolveDrainedFeedSession(sessionId: string): void {
    if (this.hasFeedWork(sessionId)) return;
    const waiters = this.feedDrainWaiters.get(sessionId) || [];
    this.feedDrainWaiters.delete(sessionId);
    for (const resolve of waiters) resolve();
  }

  private evictPendingStartForCompletion(sessionId: string | null): boolean {
    for (const key of this.pendingFeedKeysBySession.get(sessionId) || []) {
      if (!this.pendingFeedCommands.get(key)?.terminal) {
        this.removePendingFeed(key);
        return true;
      }
    }
    return false;
  }

  private failTerminalFeedForSession(sessionId: string): void {
    const active = this.activeFeeds.get(sessionId)?.command;
    if (active?.terminal) active.onFailure?.();
    for (const command of this.priorityFeedCommands.get(sessionId) || []) {
      if (command.terminal) command.onFailure?.();
    }
    for (const key of this.pendingFeedKeysBySession.get(sessionId) || []) {
      const command = this.pendingFeedCommands.get(key);
      if (command?.terminal) command.onFailure?.();
    }
  }

  private evictAnyPendingStart(): boolean {
    for (const [key, command] of this.pendingFeedCommands) {
      if (!command.terminal) {
        this.removePendingFeed(key);
        return true;
      }
    }
    return false;
  }

  private compactPendingCompletion(command: PiFeedCommand): boolean {
    const sessionId = command.context.sessionId;
    const keys = this.pendingFeedKeysBySession.get(sessionId) || [];
    for (let index = keys.length - 1; index >= 0; index -= 1) {
      const key = keys[index];
      const pending = this.pendingFeedCommands.get(key);
      if (!pending) continue;
      if (!pending.terminal) continue;
      this.pendingFeedCommands.set(key, this.compactedTerminalCommand(pending, command));
      return true;
    }
    const priority = this.priorityFeedCommands.get(sessionId) || [];
    for (let index = priority.length - 1; index >= 0; index -= 1) {
      const pending = priority[index];
      if (!pending.terminal) continue;
      priority[index] = this.compactedTerminalCommand(pending, command);
      return true;
    }
    return false;
  }

  private compactedTerminalCommand(existing: PiFeedCommand, incoming: PiFeedCommand): PiFeedCommand {
    const existingPayload = { ...existing.payload };
    const incomingPayload = incoming.payload;
    const existingSummaries = Array.isArray(existingPayload.cmux_compacted_terminal_events)
      ? existingPayload.cmux_compacted_terminal_events
      : [piTerminalFeedSummary(existingPayload)];
    const incomingSummaries = Array.isArray(incomingPayload.cmux_compacted_terminal_events)
      ? incomingPayload.cmux_compacted_terminal_events
      : [piTerminalFeedSummary(incomingPayload)];
    const existingCount = this.compactedTerminalCount(existingPayload, existingSummaries.length);
    const incomingCount = this.compactedTerminalCount(incomingPayload, incomingSummaries.length);
    const combined = [...existingSummaries, ...incomingSummaries];
    const summaryLimit = PiCmuxCommandDispatcher.maxCompactedTerminalSummaries;
    const summaries = combined.length <= summaryLimit
      ? combined
      : [...combined.slice(0, summaryLimit / 2), ...combined.slice(-summaryLimit / 2)];
    const totalCount = existingCount + incomingCount;
    delete existingPayload.tool_input;
    delete existingPayload.tool_result;
    existingPayload.cmux_compacted_terminal_count = totalCount;
    existingPayload.cmux_compacted_terminal_omitted_count = Math.max(0, totalCount - summaries.length);
    existingPayload.cmux_compacted_terminal_events = summaries;
    return { ...existing, payload: existingPayload };
  }

  private compactedTerminalCount(payload: Record<string, unknown>, fallback: number): number {
    const count = payload.cmux_compacted_terminal_count;
    return typeof count === "number" && Number.isFinite(count) && count >= fallback ? count : fallback;
  }
  private discardFeedForSession(sessionId: string): void {
    for (const key of this.pendingFeedKeysBySession.get(sessionId) || []) {
      this.pendingFeedCommands.delete(key);
    }
    this.pendingFeedKeysBySession.delete(sessionId);
    this.priorityFeedCommands.delete(sessionId);
    this.scheduledFeedSessions.delete(sessionId);
    this.feedSessionQueue = this.feedSessionQueue.filter((queued) => queued !== sessionId);
    const active = this.activeFeeds.get(sessionId);
    if (active) {
      active.cancellation.cancelled = true;
      active.cancellation.cancel?.();
    }
    this.resolveDrainedFeedSession(sessionId);
  }
  private scheduleFeed(sessionId: string | null): void {
    if (sessionId && !this.canDispatch(sessionId)) {
      this.failTerminalFeedForSession(sessionId);
      this.discardFeedForSession(sessionId);
      return;
    }
    if (!this.activeFeeds.has(sessionId) && this.queuedFeedCount(sessionId) > 0 &&
        !this.scheduledFeedSessions.has(sessionId)) {
      this.scheduledFeedSessions.add(sessionId);
      this.feedSessionQueue.push(sessionId);
    }
    this.startScheduledFeeds();
  }

  private startScheduledFeeds(): void {
    while (this.activeFeeds.size < PiCmuxCommandDispatcher.maxActiveFeedCommands) {
      const sessionId = this.feedSessionQueue.shift();
      if (sessionId === undefined) return;
      this.scheduledFeedSessions.delete(sessionId);
      if (this.activeFeeds.has(sessionId)) continue;
      const command = this.takeNextFeed(sessionId);
      if (!command) {
        if (sessionId) this.resolveDrainedFeedSession(sessionId);
        continue;
      }
      const cancellation: PiCommandCancellation = { cancelled: false };
      this.activeFeeds.set(sessionId, { cancellation, command });
      const input = boundedPiFeedInput(command.payload, PiCmuxCommandDispatcher.maxFeedInputBytes);
      void this.execute(command.args, command.cwd, input, command.context, cancellation)
      .then((result) => {
        if (result.ok && command.context.sessionId) {
          rememberSurfaceTarget(this, command.context.sessionId, result);
        }
        if (result.surfaceUnavailable) {
          const sessionId = command.context.sessionId;
          if (sessionId) {
            this.failTerminalFeedForSession(sessionId);
            this.discardFeedForSession(sessionId);
          }
        } else if (result.error instanceof Error && result.error.message.includes("timed out after")) {
          const sessionId = command.context.sessionId;
          if (sessionId) {
            this.failTerminalFeedForSession(sessionId);
            this.discardFeedForSession(sessionId);
          }
        } else if (!result.ok && command.terminal && !result.surfaceUnavailable && !cancellation.cancelled) {
          command.onFailure?.();
        }
      })
      .catch(() => {})
      .finally(() => {
        if (this.activeFeeds.get(sessionId)?.cancellation === cancellation) this.activeFeeds.delete(sessionId);
        this.scheduleFeed(sessionId);
        if (sessionId) this.resolveDrainedFeedSession(sessionId);
      });
    }
  }

  private async execute(
    args: string[],
    cwd: string,
    input: string | undefined,
    context: PiExtensionContextSnapshot,
    cancellation?: PiCommandCancellation,
  ): Promise<CommandResult> {
    const sessionId = context.sessionId;
    if (!this.canDispatch(sessionId)) {
      return this.surfaceUnavailableResult();
    }

    const result = await this.spawnCmux(args, cwd, input, cancellation);
    if (this.isSurfaceResolutionFailure(result)) {
      const shouldWarn = !sessionId || !this.unavailableSessions.has(sessionId);
      if (sessionId) this.unavailableSessions.add(sessionId);
      if (shouldWarn) {
        warn(context, "cmux hook command failed", {
          status: result.status,
          stderr_available: result.stderr.trim().length > 0,
          error_available: result.error !== undefined,
          surface_unavailable: true,
          dispatch_disabled: true,
        });
      }
      return { ...result, surfaceUnavailable: true };
    }
    return result;
  }

  private spawnCmux(
    args: string[],
    cwd: string,
    input?: string,
    cancellation?: PiCommandCancellation,
  ): Promise<CommandResult> {
    return new Promise<CommandResult>((resolve) => {
      let settled = false;
      let stdout = "";
      let stderr = "";
      let inputError: unknown;
      let timeout: ReturnType<typeof setTimeout> | null = null;
      let terminateGrace: ReturnType<typeof setTimeout> | null = null;
      let forceSettleTimeout: ReturnType<typeof setTimeout> | null = null;
      let terminationError: Error | undefined;

      const appendOutput = (current: string, chunk: unknown): string => {
        const limit = 1024 * 1024;
        if (current.length >= limit) return current;
        return current + String(chunk).slice(0, limit - current.length);
      };
      const settle = (result: CommandResult) => {
        if (settled) return;
        settled = true;
        if (timeout) clearTimeout(timeout);
        if (terminateGrace) clearTimeout(terminateGrace);
        if (forceSettleTimeout) clearTimeout(forceSettleTimeout);
        if (cancellation) cancellation.cancel = undefined;
        resolve(result);
      };
      const terminatedResult = (): CommandResult => ({
        ok: false,
        status: null,
        stdout,
        stderr,
        error: terminationError,
      });

      try {
        const child = spawn(cmuxExecutable(), args, {
          env: hookEnvironment(cwd, true),
          stdio: ["pipe", "pipe", "pipe"],
        });
        child.stdout.setEncoding("utf8");
        child.stderr.setEncoding("utf8");
        child.stdout.on("data", (chunk) => {
          stdout = appendOutput(stdout, chunk);
        });
        child.stderr.on("data", (chunk) => {
          stderr = appendOutput(stderr, chunk);
        });
        child.stdin.on("error", (error) => {
          inputError = error;
        });
        const beginTermination = (error: Error) => {
          if (terminationError) return;
          terminationError = error;
          child.stdin.destroy();
          try {
            child.kill("SIGTERM");
          } catch (_) {}
          terminateGrace = setTimeout(() => {
            try {
              child.kill("SIGKILL");
            } catch (_) {}
            forceSettleTimeout = setTimeout(() => {
              child.stdout.destroy();
              child.stderr.destroy();
              child.unref();
              settle(terminatedResult());
            }, 250);
          }, 250);
        };
        child.on("error", (error) => {
          settle(terminationError ? terminatedResult() : { ok: false, status: null, stdout, stderr, error });
        });
        child.on("close", (code) => {
          if (terminationError) {
            settle(terminatedResult());
            return;
          }
          const status = typeof code === "number" ? code : null;
          settle({
            ok: status === 0 && inputError === undefined,
            status,
            stdout,
            stderr,
            error: inputError,
          });
        });
        if (cancellation) {
          cancellation.cancel = () => beginTermination(new Error("cmux feed command cancelled"));
          if (cancellation.cancelled) cancellation.cancel();
        }
        timeout = setTimeout(() => {
          beginTermination(new Error("cmux command timed out after 5000ms"));
        }, 5000);
        child.stdin.end(input);
      } catch (error) {
        settle({ ok: false, status: null, stdout, stderr, error });
      }
    });
  }

  private isSurfaceResolutionFailure(result: CommandResult): boolean {
    return !result.ok && result.status === PiCmuxCommandDispatcher.surfaceUnavailableExitCode;
  }

  private surfaceUnavailableResult(): CommandResult {
    return {
      ok: false,
      status: null,
      stdout: "",
      stderr: "",
      surfaceUnavailable: true,
    };
  }
}

async function sendHook(
  dispatcher: PiCmuxCommandDispatcher,
  subcommand: string,
  context: PiExtensionContextSnapshot,
  extra: HookExtra = {},
): Promise<boolean> {
  if (process.env.CMUX_PI_HOOKS_DISABLED === "1") return true;
  const sessionId = context.sessionId;
  if (!sessionId) return true;
  const target = surfaceTargetArgs(dispatcher, sessionId);
  if (!target) return !firstString(process.env.CMUX_PANEL_ID);

  const cwd = context.cwd;
  const payload: HookExtra = {
    session_id: sessionId,
    cwd,
    hook_event_name: eventName(subcommand),
    event: eventName(subcommand),
    ...extra,
  };
  const result = await dispatcher.run(
    ["hooks", "pi", subcommand, ...target],
    cwd,
    JSON.stringify(payload),
    context,
  );
  if (result.ok) rememberSurfaceTarget(dispatcher, sessionId, result);
  if (!result.ok && !result.surfaceUnavailable) {
    warn(context, "cmux hook command failed", {
      subcommand,
      status: result.status,
      stderr_available: result.stderr.trim().length > 0,
      error_available: result.error !== undefined,
    });
  }
  return result.ok;
}

const resolvedSurfaceTargets = new WeakMap<PiCmuxCommandDispatcher, Map<string, string[]>>();

function surfaceTargetsFor(dispatcher: PiCmuxCommandDispatcher): Map<string, string[]> {
  let targets = resolvedSurfaceTargets.get(dispatcher);
  if (!targets) {
    targets = new Map();
    resolvedSurfaceTargets.set(dispatcher, targets);
  }
  return targets;
}

function surfaceTargetArgs(dispatcher: PiCmuxCommandDispatcher, sessionId: string): string[] | null {
  const resolved = surfaceTargetsFor(dispatcher).get(sessionId);
  if (resolved) return [...resolved];
  const surfaceId = firstString(process.env.CMUX_SURFACE_ID);
  if (!surfaceId) return null;
  const args: string[] = [];
  const workspaceId = firstString(process.env.CMUX_WORKSPACE_ID);
  if (workspaceId) args.push("--workspace", workspaceId);
  args.push("--surface", surfaceId);
  return args;
}

function rememberSurfaceTarget(
  dispatcher: PiCmuxCommandDispatcher,
  sessionId: string,
  result: CommandResult,
): void {
  const payload = parseJSONOutput(result);
  const workspaceId = firstString(payload?.workspace_id);
  const surfaceId = firstString(payload?.surface_id);
  if (!workspaceId || !surfaceId) return;
  surfaceTargetsFor(dispatcher).set(
    sessionId,
    ["--workspace", workspaceId, "--surface", surfaceId],
  );
}

function releaseSessionRuntime(
  dispatcher: PiCmuxCommandDispatcher,
  sessionStates: Map<string, SessionState>,
  sessionId: string,
): void {
  dispatcher.releaseSession(sessionId);
  sessionStates.delete(sessionId);
  surfaceTargetsFor(dispatcher).delete(sessionId);
}

function parseJSONOutput(result: CommandResult): Record<string, unknown> | null {
  if (!result.ok) return null;
  try {
    const parsed = JSON.parse(result.stdout);
    return parsed && typeof parsed === "object" && !Array.isArray(parsed) ? parsed as Record<string, unknown> : null;
  } catch (_) {
    return null;
  }
}

function resumeBindingMatches(payload: Record<string, unknown> | null, sessionId: string): boolean {
  const binding = payload?.resume_binding;
  if (!binding || typeof binding !== "object") return false;
  const typed = binding as Record<string, unknown>;
  return firstString(typed.kind) === "pi" &&
    firstString(typed.checkpoint_id, typed.checkpointId) === sessionId;
}

const piOptionsWithValue = new Set([
  "--model",
  "-m",
  "--thinking",
  "--provider",
  "--extension",
  "-e",
  "--skill",
  "--mcp-config",
  "--permission-mode",
  "--session-dir",
  "--config",
  "--profile",
  "--system-prompt",
  "--append-system-prompt",
  "--cwd",
  "--dir",
  "--trust",
  "--sandbox",
]);

const piOptionsWithoutValue = new Set([
  "--no-color",
  "--dangerously-skip-permissions",
  "--yolo",
]);

const piSelectorsToDrop = new Set([
  "--session",
  "-s",
  "--resume",
  "--fork",
  "--api-key",
  "--prompt",
  "--print",
]);

function sanitizedResumeArgv(sessionId: string): string[] {
  const raw = normalizedLaunchArgv();
  const executable = raw[0] || resolveExecutable("pi");
  const out = [executable, "--session", sessionId];
  for (let index = 1; index < raw.length; index += 1) {
    const arg = raw[index];
    if (!arg) continue;
    if (piSelectorsToDrop.has(arg)) {
      if (index + 1 < raw.length && !raw[index + 1].startsWith("-")) index += 1;
      continue;
    }
    if (
      arg.startsWith("--session=") ||
      arg.startsWith("--resume=") ||
      arg.startsWith("--fork=") ||
      arg.startsWith("--api-key=") ||
      arg.startsWith("--prompt=")
    ) {
      continue;
    }
    if (piOptionsWithValue.has(arg)) {
      out.push(arg);
      if (index + 1 < raw.length) {
        out.push(raw[index + 1]);
        index += 1;
      }
      continue;
    }
    if ([...piOptionsWithValue].some((option) => arg.startsWith(`${option}=`)) || piOptionsWithoutValue.has(arg)) {
      out.push(arg);
    }
  }
  return out;
}

async function ensureResumeBinding(
  dispatcher: PiCmuxCommandDispatcher,
  context: PiExtensionContextSnapshot,
  sessionId: string,
): Promise<void> {
  if (process.env.CMUX_PI_HOOKS_DISABLED === "1") return;
  const target = surfaceTargetArgs(dispatcher, sessionId);
  if (!target) return;

  const cwd = context.cwd;
  const resumeArgv = sanitizedResumeArgv(sessionId);
  const set = await dispatcher.run([
    "--json",
    "surface",
    "resume",
    "set",
    ...target,
    "--name",
    "Pi",
    "--kind",
    "pi",
    "--checkpoint-id",
    sessionId,
    "--source",
    "agent-hook",
    "--cwd",
    cwd,
    "--",
    ...resumeArgv,
  ], cwd, undefined, context);
  if (!set.ok && !set.surfaceUnavailable) {
    warn(context, "failed to set Pi resume binding", {
      status: set.status,
      stderr_available: set.stderr.trim().length > 0,
      error_available: set.error !== undefined,
    });
    return;
  }
  if (set.surfaceUnavailable) return;

  const verification = await dispatcher.run(
    ["--json", "surface", "resume", "get", ...target],
    cwd,
    undefined,
    context,
  );
  if (verification.surfaceUnavailable) return;
  const verified = parseJSONOutput(verification);
  if (!resumeBindingMatches(verified, sessionId)) {
    warn(context, "Pi resume binding did not verify after write", { session_id: sessionId });
  }
}

async function clearResumeBinding(
  dispatcher: PiCmuxCommandDispatcher,
  context: PiExtensionContextSnapshot,
  sessionId: string,
): Promise<void> {
  if (process.env.CMUX_PI_HOOKS_DISABLED === "1") return;
  const target = surfaceTargetArgs(dispatcher, sessionId);
  if (!target) return;
  const cwd = context.cwd;
  const result = await dispatcher.run([
    "--json",
    "surface",
    "resume",
    "clear",
    ...target,
    "--checkpoint-id",
    sessionId,
    "--source",
    "agent-hook",
  ], cwd, undefined, context);
  if (result.surfaceUnavailable) return;
  if (!result.ok) {
    warn(context, "failed to clear Pi resume binding", {
      status: result.status,
      stderr_available: result.stderr.trim().length > 0,
      error_available: result.error !== undefined,
    });
  }
}

type PiFeedEventName =
  | "PreToolUse"
  | "PostToolUse"
  | "PreCompact"
  | "PostCompact"
  | "SubagentStart"
  | "SubagentStop";

const subagentToolNames = new Set([
  "subagent",
  "team_spawn",
  "superpowers_dispatch",
  "Task",
]);

function isSubagentTool(event: unknown): boolean {
  const toolName = firstString(objectValue(event, ["toolName", "tool_name", "name"]));
  return toolName !== null && (subagentToolNames.has(toolName) || /subagent/i.test(toolName));
}

function isTerminalFeedEvent(eventName: PiFeedEventName): boolean {
  return eventName === "PostToolUse" || eventName === "SubagentStop";
}

function prepareFeedDispatch(
  dispatcher: PiCmuxCommandDispatcher,
  sessionStates: Map<string, SessionState>,
  eventName: PiFeedEventName,
  context: PiExtensionContextSnapshot,
  event: unknown,
): (() => void) | undefined {
  if (process.env.CMUX_PI_HOOKS_DISABLED === "1") return undefined;
  const sessionId = context.sessionId;
  if (!sessionId) return undefined;
  if (!dispatcher.canDispatch(sessionId)) return undefined;
  const state = stateFor(sessionStates, sessionId);
  if (state.stopped) return undefined;
  const cwd = context.cwd;
  const toolCallId = firstString(objectValue(event, ["toolCallId", "tool_call_id", "id"]));
  const toolName = firstString(objectValue(event, ["toolName", "tool_name", "name"]));
  const projectionState: PiFeedProjectionState = { remainingNodes: 48, seen: new WeakSet() };
  const payload: HookExtra = {
    session_id: utf8Prefix(sessionId, 256),
    cwd: utf8Prefix(cwd, 2048),
    hook_event_name: eventName,
    event: eventName,
    turn_id: utf8Prefix(currentTurnId(sessionStates, sessionId, event), 256),
  };
  const boundedToolCallId = utf8Prefix(toolCallId, 256);
  if (boundedToolCallId !== undefined) payload.tool_call_id = boundedToolCallId;
  const boundedToolName = utf8Prefix(toolName, 256);
  if (boundedToolName !== undefined) payload.tool_name = boundedToolName;
  const toolInput = objectValue(event, ["args", "input"]);
  if (toolInput !== undefined) payload.tool_input = projectPiFeedValue(toolInput, projectionState);
  if (isTerminalFeedEvent(eventName)) {
    const toolResult = objectValue(event, ["result", "details", "content"]);
    if (toolResult !== undefined) {
      payload.tool_result = projectPiFeedValue(toolResult, projectionState, 0, false);
    }
    const isError = objectValue(event, ["isError", "is_error"]);
    if (isError !== undefined) payload.is_error = projectPiFeedValue(isError, projectionState);
  }
  return () => {
    const target = surfaceTargetArgs(dispatcher, sessionId);
    if (!target) return;
    dispatcher.enqueueFeed(`${sessionId}:${toolCallId || toolName || "unknown"}`, {
      args: ["hooks", "feed", "--source", "pi", "--event", eventName, ...target],
      cwd,
      payload,
      context,
      terminal: isTerminalFeedEvent(eventName),
      onFailure: () => { state.feedDeliveryFailed = true; },
    });
  };
}

async function publishPendingCompletion(
  dispatcher: PiCmuxCommandDispatcher,
  sessionStates: Map<string, SessionState>,
  context: PiExtensionContextSnapshot,
  sessionId: string,
  completion: PendingCompletion,
): Promise<void> {
  await dispatcher.finishFeedForSession(sessionId);
  const state = stateFor(sessionStates, sessionId);
  const feedDelivered = !state.feedDeliveryFailed;
  state.feedDeliveryFailed = false;
  if (!feedDelivered) {
    warn(context, "cmux hook command failed", { session_id: sessionId });
  }
  const stopPayload: HookExtra = {
    last_assistant_message: completion.lastAssistantMessage,
    turn_id: completion.turnId,
  };
  if (feedDelivered) {
    const notificationRouted = await sendHook(dispatcher, "notification", context, {
      message: completion.lastAssistantMessage || "Task completed",
      turn_id: completion.turnId,
      notification: { type: completion.notificationType },
    });
    if (notificationRouted) stopPayload.cmux_notification_routed = true;
  }
  await sendHook(dispatcher, "stop", context, stopPayload);
}

export default function cmuxPiSessionExtension(pi: ExtensionAPI) {
  const dispatcher = new PiCmuxCommandDispatcher();
  const sessionStates = new Map<string, SessionState>();
  const lifecycleTails = new Map<string, Promise<void>>();

  const enqueueLifecycleTask = (
    sessionId: string,
    context: PiExtensionContextSnapshot,
    operation: () => Promise<unknown> | unknown,
  ): Promise<void> => {
    const previous = lifecycleTails.get(sessionId) || Promise.resolve();
    let tracked: Promise<void>;
    tracked = previous
      .then(operation)
      .then(() => undefined)
      .catch((error) => {
        const errorMessage = error instanceof Error ? error.message : undefined;
        warn(context, "cmux lifecycle task failed", {
          error_available: error !== undefined,
          error_message: utf8Prefix(errorMessage, 512),
        }, true);
      })
      .finally(() => {
        if (lifecycleTails.get(sessionId) === tracked) lifecycleTails.delete(sessionId);
      });
    lifecycleTails.set(sessionId, tracked);
    return tracked;
  };

  pi.on("session_start", (_event, ctx) => {
    const context = snapshotContext(ctx);
    const sessionId = context.sessionId;
    if (sessionId) {
      const state = stateFor(sessionStates, sessionId);
      state.pendingCompletion = undefined;
      state.feedDeliveryFailed = false;
      state.stopped = false;
    }
    if (!sessionId) return;
    enqueueLifecycleTask(sessionId, context, async () => {
      const ok = await sendHook(dispatcher, "session-start", context);
      if (ok) await ensureResumeBinding(dispatcher, context, sessionId);
    });
  });

  pi.on("before_agent_start", (event, ctx) => {
    const context = snapshotContext(ctx);
    const sessionId = context.sessionId;
    if (!sessionId) return;
    const turnId = beginTurn(sessionStates, sessionId, event);
    enqueueLifecycleTask(sessionId, context, () => (
      sendHook(dispatcher, "prompt-submit", context, { prompt: event.prompt, turn_id: turnId })
    ));
  });

  const enqueueFeed = (
    eventName: PiFeedEventName,
    event: unknown,
    ctx: ExtensionContext,
  ): void => {
    const context = snapshotContext(ctx);
    const sessionId = context.sessionId;
    if (!sessionId) return;
    const dispatch = prepareFeedDispatch(dispatcher, sessionStates, eventName, context, event);
    if (!dispatch) return;
    enqueueLifecycleTask(sessionId, context, dispatch);
  };

  pi.on("tool_execution_start", (event, ctx) => {
    enqueueFeed(isSubagentTool(event) ? "SubagentStart" : "PreToolUse", event, ctx);
  });

  pi.on("tool_execution_end", (event, ctx) => {
    enqueueFeed(isSubagentTool(event) ? "SubagentStop" : "PostToolUse", event, ctx);
  });

  pi.on("session_before_compact", (event, ctx) => {
    enqueueFeed("PreCompact", event, ctx);
  });

  pi.on("session_compact", (event, ctx) => {
    enqueueFeed("PostCompact", event, ctx);
  });

  pi.on("agent_end", (event, ctx) => {
    const context = snapshotContext(ctx);
    const sessionId = context.sessionId;
    if (!sessionId) return;
    const state = stateFor(sessionStates, sessionId);
    const message = lastAssistantMessage(event);
    // Preserve the latest low-level result until Pi confirms no automatic work remains.
    state.pendingCompletion = {
      lastAssistantMessage: message || state.pendingCompletion?.lastAssistantMessage,
      notificationType: firstString(objectValue(event, ["stopReason", "reason", "terminationReason"])) || "completed",
      turnId: currentTurnId(sessionStates, sessionId, event),
    };
    // Older Pi versions do not emit agent_settled, so retain their established completion behavior.
    if (!supportsAgentSettled()) {
      const completion = settleTurn(sessionStates, sessionId);
      if (completion) {
        enqueueLifecycleTask(sessionId, context, () => (
          publishPendingCompletion(dispatcher, sessionStates, context, sessionId, completion)
        ));
      }
    }
  });

  pi.on("agent_settled", (_event, ctx) => {
    const context = snapshotContext(ctx);
    const isIdle = ctx.isIdle();
    const sessionId = context.sessionId;
    if (!sessionId || !isIdle) return;
    // Consume pending completion before subprocess calls so duplicate settlement cannot notify twice.
    const completion = settleTurn(sessionStates, sessionId);
    if (completion) {
      enqueueLifecycleTask(sessionId, context, () => (
        publishPendingCompletion(dispatcher, sessionStates, context, sessionId, completion)
      ));
    }
  });

  pi.on("session_shutdown", async (event, ctx) => {
    const context = snapshotContext(ctx);
    const sessionId = context.sessionId;
    if (!sessionId) return;
    const state = stateFor(sessionStates, sessionId);
    let stopPayload: HookExtra | undefined;
    if (!state.stopped) {
      const turnId = finishTurn(sessionStates, sessionId, event);
      stopPayload = {
        turn_id: turnId,
        terminationReason: firstString(objectValue(event, ["reason"])) || "session_shutdown",
      };
    }
    await enqueueLifecycleTask(sessionId, context, async () => {
      await dispatcher.finishFeedForSession(sessionId);
      const feedDelivered = !state.feedDeliveryFailed;
      state.feedDeliveryFailed = false;
      if (!feedDelivered) warn(context, "cmux hook command failed", { session_id: sessionId });
      if (stopPayload) await sendHook(dispatcher, "stop", context, stopPayload);
      try {
        await clearResumeBinding(dispatcher, context, sessionId);
      } finally {
        releaseSessionRuntime(dispatcher, sessionStates, sessionId);
      }
    });
  });
}