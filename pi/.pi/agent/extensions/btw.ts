/**
 * /btw — side-chat that doesn't bloat the main thread.
 *
 * Inspired by Claude Code's /btw and https://github.com/patriceckhart/pi-btw.
 * Opens an interactive side chat. The LLM sees your full main-thread context and
 * answers, but nothing is written back into the main conversation history — so
 * follow-up questions don't cost you tokens on every subsequent main-thread turn.
 *
 * Usage:
 *   /btw                          — open the side chat (type questions interactively)
 *   /btw <question>               — seed an initial question
 *
 * Install: this file lives in ~/.pi/agent/extensions/btw.ts (global, hot-reloadable).
 */

import { complete } from "@earendil-works/pi-ai/compat";
import type { ExtensionAPI, SessionEntry } from "@earendil-works/pi-coding-agent";
import { convertToLlm, getMarkdownTheme } from "@earendil-works/pi-coding-agent";
import {
	type Component,
	type Focusable,
	Input,
	Loader,
	Markdown,
	matchesKey,
	truncateToWidth,
	wrapTextWithAnsi,
} from "@earendil-works/pi-tui";

interface BtwMessage {
	role: "user" | "assistant";
	text: string;
}

export default function btwExtension(pi: ExtensionAPI) {
	pi.registerCommand("btw", {
		description: "Open a side-chat that doesn't add to the main thread (saves tokens)",
		handler: async (args, ctx) => {
			if (ctx.mode !== "tui") {
				ctx.ui.notify("/btw requires interactive mode", "error");
				return;
			}

			if (!ctx.model) {
				ctx.ui.notify("No model selected", "error");
				return;
			}

			// Freeze the main conversation context at entry.
			const branch = ctx.sessionManager.getBranch();
			const mainMessages = branch
				.filter((entry): entry is SessionEntry & { type: "message" } => entry.type === "message")
				.map((entry) => entry.message);
			const mainLlmMessages = convertToLlm(mainMessages);
			const systemPrompt = ctx.getSystemPrompt();

			const btwHistory: BtwMessage[] = [];
			let isLoading = false;
			let loadingAbort: AbortController | null = null;

			const initialQuestion = args.trim();

			await ctx.ui.custom<void>((tui, theme, _kb, done) => {
				const mdTheme = getMarkdownTheme();
				const input = new Input();
				input.focused = true;
				let loader: Loader | null = null;

				const chatUI: Component & Focusable = {
					focused: true,

					handleInput(data: string) {
						// Esc: abort in-flight request, otherwise close.
						if (matchesKey(data, "escape")) {
							if (isLoading && loadingAbort) {
								loadingAbort.abort();
								isLoading = false;
								loadingAbort = null;
								if (loader) {
									loader.stop();
									loader = null;
								}
								tui.requestRender();
							} else {
								done();
							}
							return;
						}

						input.handleInput(data);
						tui.requestRender();
					},

					render(width: number): string[] {
						const lines: string[] = [];

						const headerText =
							theme.fg("accent", theme.bold(" btw ")) + theme.fg("dim", "— side chat (Esc to close)");
						const bdr = theme.fg("border", "─".repeat(width));
						lines.push(bdr);
						lines.push(truncateToWidth(headerText, width));
						lines.push(bdr);

						for (const msg of btwHistory) {
							if (msg.role === "user") {
								lines.push("");
								const label = theme.fg("accent", theme.bold("you: "));
								const wrapped = wrapTextWithAnsi(label + msg.text, width - 2);
								for (const wl of wrapped) {
									lines.push(truncateToWidth(` ${wl}`, width));
								}
							} else {
								lines.push("");
								const md = new Markdown(msg.text, 1, 0, mdTheme);
								lines.push(...md.render(width));
							}
						}

						if (isLoading && loader) {
							lines.push(...loader.render(width));
						}

						lines.push("");
						lines.push(bdr);
						lines.push(...input.render(width));

						return lines;
					},

					invalidate() {},
				};

				input.onSubmit = (value: string) => {
					const question = value.trim();
					if (!question || isLoading) return;

					btwHistory.push({ role: "user", text: question });
					input.setValue("");
					isLoading = true;
					loader = new Loader(
						tui,
						(s) => theme.fg("accent", s),
						(s) => theme.fg("muted", s),
						"Working...",
					);
					tui.requestRender();

					const btwLlmMessages = btwHistory.map((m) => ({
						role: m.role,
						content: [{ type: "text" as const, text: m.text }],
						timestamp: Date.now(),
					}));

					loadingAbort = new AbortController();
					const signal = loadingAbort.signal;

					const doComplete = async () => {
						const auth = await ctx.modelRegistry.getApiKeyAndHeaders(ctx.model!);
						if (!auth.ok || !auth.apiKey) {
							throw new Error(auth.ok ? `No API key for ${ctx.model!.provider}` : auth.error);
						}

						const response = await complete(
							ctx.model!,
							{
								systemPrompt,
								messages: [...mainLlmMessages, ...btwLlmMessages],
							},
							{
								apiKey: auth.apiKey,
								headers: auth.headers,
								env: auth.env,
								signal,
							},
						);

						if (response.stopReason === "aborted") return;

						const answer = response.content
							.filter((c): c is { type: "text"; text: string } => c.type === "text")
							.map((c) => c.text)
							.join("\n");

						btwHistory.push({ role: "assistant", text: answer });
					};

					doComplete()
						.catch((err: unknown) => {
							if (signal.aborted) return;
							const message = err instanceof Error ? err.message : String(err);
							btwHistory.push({ role: "assistant", text: `Error: ${message}` });
						})
						.finally(() => {
							isLoading = false;
							loadingAbort = null;
							if (loader) {
								loader.stop();
								loader = null;
							}
							tui.requestRender();
						});
				};

				if (initialQuestion) {
					input.setValue(initialQuestion);
					input.onSubmit(initialQuestion);
				}

				return chatUI;
			});

			// Nothing is written back to the main session.
		},
	});
}
