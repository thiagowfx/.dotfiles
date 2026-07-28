import assert from "node:assert/strict";
import test from "node:test";
import install, { moonForPercent } from "../context-moon.ts";

test("uses accent color for every context usage level", async () => {
  const handlers = new Map<string, (event: unknown, ctx: unknown) => void>();
  install({
    on(event: string, handler: (event: unknown, ctx: unknown) => void) {
      handlers.set(event, handler);
    },
  } as never);

  for (const percent of [0, 50, 75, 100]) {
    const statuses: string[] = [];
    handlers.get("session_start")?.({}, {
      getContextUsage: () => ({ percent }),
      ui: {
        setStatus(_key: string, text: string) {
          statuses.push(text);
        },
        theme: { fg: (color: string, text: string) => `${color}(${text})` },
      },
    });

    assert.equal(statuses.at(-1), `accent([${moonForPercent(percent)} ${percent.toFixed(0)}%])`);
  }
});

test("uses accent color when context usage is unknown", () => {
  const handlers = new Map<string, (event: unknown, ctx: unknown) => void>();
  install({
    on(event: string, handler: (event: unknown, ctx: unknown) => void) {
      handlers.set(event, handler);
    },
  } as never);

  const statuses: string[] = [];
  handlers.get("session_start")?.({}, {
    getContextUsage: () => ({ percent: null }),
    ui: {
      setStatus(_key: string, text: string) {
        statuses.push(text);
      },
      theme: { fg: (color: string, text: string) => `${color}(${text})` },
    },
  });

  assert.equal(statuses.at(-1), "accent([🌙 ?])");
});
