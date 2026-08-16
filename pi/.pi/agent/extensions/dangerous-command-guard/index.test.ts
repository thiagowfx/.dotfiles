import assert from "node:assert/strict";
import test from "node:test";

import bootstrapDangerousCommandGuard, { ensureDependencies } from "./index.ts";
import dangerousCommandGuard, { findBlockedCommand } from "./guard.ts";

const blocked = [
  ["rm -rf ./build", "rm -rf"],
  ["rm -r -f ./build", "rm -rf"],
  ["/bin/rm '-rf' ./build", "rm -rf"],
  ["command rm -fr ./build", "rm -rf"],
  ["env -S 'rm -rf' ./build", "rm -rf"],
  ["sudo -u root rm -rf ./build", "rm -rf"],
  ["nohup rm -rf ./build", "rm -rf"],
  ["nice -n 10 rm -rf ./build", "rm -rf"],
  ["find . -exec rm -rf {} +", "rm -rf"],
  ["printf '%s\\0' /tmp/example | xargs -0 rm -rf", "rm -rf"],
  ["parallel rm -rf ::: /tmp/example", "rm -rf"],
  ["eval 'rm -rf ./build'", "rm -rf"],
  ["rm -rf /tmp/../etc", "rm -rf"],
  ["terraform -chdir=infra apply", "terraform apply"],
  ["terraform destroy", "terraform destroy"],
  ["just --justfile ops.just apply", "just apply"],
  ["just destroy", "just destroy"],
  ["tofu apply --auto-approve", "--auto-approve"],
  ["rm -r ~/.cache/pre-commit", "pre-commit cache"],
  ["git push --force origin main", "Force-push"],
  ["git -C repo push origin HEAD:refs/heads/master --force-with-lease", "Force-push"],
  ["git reset HEAD --hard", "git reset --hard"],
  ["git clean -fdx", "git clean"],
  ["git commit -m test --no-verify", "--no-verify"],
  ["git push --no-verify origin feature", "--no-verify"],
  ["bash -c 'terraform destroy'", "terraform destroy"],
  ["bash <<'EOF'\nrm -rf ./build\nEOF", "rm -rf"],
  ["echo $(git reset --hard HEAD)", "git reset --hard"],
] as const;

for (const [command, reasonPart] of blocked) {
  test(`blocks: ${command}`, async () => {
    const result = await findBlockedCommand(command);
    assert.ok(result, `expected command to be blocked: ${command}`);
    assert.match(result.reason, new RegExp(reasonPart, "i"));
  });
}

const allowed = [
  "echo 'rm -rf /tmp/example'",
  "printf '%s\\n' 'terraform destroy'",
  "cat <<'EOF'\nrm -rf /tmp/example\nEOF",
  "# git reset --hard HEAD\ngit status",
  "rm -r /tmp/example",
  "rm -f /tmp/example",
  "rm /tmp/notes-on-.cache/pre-commit-migration.md",
  "rm -rf /tmp/example",
  "rm -r -f /tmp/example",
  "/bin/rm '-rf' /tmp/example",
  "command rm -fr /tmp/example",
  "env -S 'rm -rf' /tmp/example",
  "sudo -u root rm -rf /tmp/example",
  "nohup rm -rf /tmp/example",
  "nice -n 10 rm -rf /tmp/example",
  "watch -n 1 rm -rf /tmp/example",
  "eval 'rm -rf /tmp/example'",
  "bash <<'EOF'\nrm -rf /tmp/example\nEOF",
  "rm -rf /tmp",
  "terraform plan",
  "just plan",
  "git push --force-with-lease origin feature",
  "git reset --soft HEAD^",
  "git clean -nfdx",
  "git commit -m 'mention --no-verify in docs'",
  "git commit -m \"$(cat <<'EOF'\nfix: dynamic message\nEOF\n)\"",
  '"$shell" /tmp/test-cdg.sh "$PWD/profile/.profile.d/alias.sh"',
  "git commit -q -F - <<'EOF' 2>&1 | tail -20\nfeat: message\nEOF",
];

for (const command of allowed) {
  test(`allows: ${command}`, async () => {
    assert.equal(await findBlockedCommand(command), undefined);
  });
}

test("fails closed when guarded command contains dynamic arguments", async () => {
  const result = await findBlockedCommand("rm $flags /tmp/example");
  assert.ok(result);
  assert.match(result.reason, /dynamic arguments/i);
});

test("reports malformed shell syntax", async () => {
  const result = await findBlockedCommand("git push '");
  assert.ok(result);
  assert.match(result.reason, /parse/i);
});

test("extension prompts before dangerous bash tool calls", async () => {
  let handler:
    | ((
        event: { toolName: string; input: { command: string } },
        context: { hasUI: boolean; ui: { confirm: (title: string, message: string) => Promise<boolean> } },
      ) => Promise<unknown>)
    | undefined;
  const pi = {
    on(event: string, callback: typeof handler) {
      assert.equal(event, "tool_call");
      handler = callback;
    },
  };

  await dangerousCommandGuard(pi as never);
  assert.ok(handler);

  const prompts: Array<[string, string]> = [];
  const allowContext = {
    hasUI: true,
    ui: {
      async confirm(title: string, message: string) {
        prompts.push([title, message]);
        return true;
      },
    },
  };
  assert.equal(
    await handler({ toolName: "bash", input: { command: "git reset --hard HEAD" } }, allowContext),
    undefined,
  );
  assert.deepEqual(prompts, [
    [
      "Allow dangerous command?",
      "git reset --hard HEAD\n\ngit reset --hard is blocked - discards changes irreversibly",
    ],
  ]);

  const denyContext = {
    hasUI: true,
    ui: { confirm: async () => false },
  };
  assert.deepEqual(
    await handler({ toolName: "bash", input: { command: "rm -rf ./build" } }, denyContext),
    { block: true, reason: "Blocked by user" },
  );

  const noUiContext = {
    hasUI: false,
    ui: { confirm: async () => assert.fail("confirmation should not run without UI") },
  };
  assert.deepEqual(
    await handler({ toolName: "bash", input: { command: "rm -rf ./build" } }, noUiContext),
    { block: true, reason: "rm -rf is blocked for safety" },
  );
  assert.equal(
    await handler({ toolName: "bash", input: { command: "rm -rf /tmp/example" } }, noUiContext),
    undefined,
  );
  assert.equal(
    await handler(
      {
        toolName: "bash",
        input: { command: '"$shell" /tmp/test-cdg.sh "$PWD/profile/.profile.d/alias.sh"' },
      },
      noUiContext,
    ),
    undefined,
  );

  assert.equal(
    await handler({ toolName: "bash", input: { command: "echo 'rm -rf /tmp/example'" } }, allowContext),
    undefined,
  );
  assert.equal(
    await handler(
      {
        toolName: "bash",
        input: { command: '"$shell" /tmp/test-cdg.sh "$PWD/profile/.profile.d/alias.sh"' },
      },
      allowContext,
    ),
    undefined,
  );
  assert.equal(prompts.length, 1);
});

test("loader installs missing dependencies before registering guard", async () => {
  let dependenciesAvailable = false;
  let handler: unknown;
  const pi = {
    async exec(command: string, arguments_: string[], options: { cwd: string }) {
      assert.equal(command, "npm");
      assert.deepEqual(arguments_, ["ci", "--ignore-scripts"]);
      assert.match(options.cwd, /dangerous-command-guard$/);
      dependenciesAvailable = true;
      return { code: 0, stdout: "", stderr: "", killed: false };
    },
    on(_event: string, callback: unknown) {
      handler = callback;
    },
  };

  await bootstrapDangerousCommandGuard(pi as never, async () => dependenciesAvailable);
  assert.ok(handler);
});

test("loader skips npm when dependencies already exist", async () => {
  let handler: unknown;
  const pi = {
    async exec() {
      throw new Error("npm should not run");
    },
    on(_event: string, callback: unknown) {
      handler = callback;
    },
  };

  await bootstrapDangerousCommandGuard(pi as never, async () => true);
  assert.ok(handler);
});

test("dependency bootstrap reports npm failures", async () => {
  const pi = {
    async exec() {
      return { code: 1, stdout: "", stderr: "network unavailable", killed: false };
    },
  };

  await assert.rejects(
    ensureDependencies(pi as never, async () => false),
    /Failed to install dangerous command guard dependencies: network unavailable/,
  );
});

test("dependency bootstrap verifies installed files", async () => {
  const pi = {
    async exec() {
      return { code: 0, stdout: "", stderr: "", killed: false };
    },
  };

  await assert.rejects(
    ensureDependencies(pi as never, async () => false),
    /required files are still missing/,
  );
});
