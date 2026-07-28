import assert from "node:assert/strict";
import test from "node:test";

import dangerousCommandGuard, { findBlockedCommand } from "./index.ts";

const blocked = [
  ["rm -rf /tmp/example", "rm -rf"],
  ["rm -r -f /tmp/example", "rm -rf"],
  ["/bin/rm '-rf' /tmp/example", "rm -rf"],
  ["command rm -fr /tmp/example", "rm -rf"],
  ["env -S 'rm -rf' /tmp/example", "rm -rf"],
  ["sudo -u root rm -rf /tmp/example", "rm -rf"],
  ["nohup rm -rf /tmp/example", "rm -rf"],
  ["nice -n 10 rm -rf /tmp/example", "rm -rf"],
  ["find . -exec rm -rf {} +", "rm -rf"],
  ["printf '%s\\0' /tmp/example | xargs -0 rm -rf", "rm -rf"],
  ["watch -n 1 rm -rf /tmp/example", "rm -rf"],
  ["parallel rm -rf ::: /tmp/example", "rm -rf"],
  ["eval 'rm -rf /tmp/example'", "rm -rf"],
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
  ["bash <<'EOF'\nrm -rf /tmp/example\nEOF", "rm -rf"],
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
  "terraform plan",
  "just plan",
  "git push --force-with-lease origin feature",
  "git reset --soft HEAD^",
  "git clean -nfdx",
  "git commit -m 'mention --no-verify in docs'",
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

test("extension blocks dangerous bash tool calls", async () => {
  let handler: ((event: { toolName: string; input: { command: string } }) => Promise<unknown>) | undefined;
  const pi = {
    on(event: string, callback: typeof handler) {
      assert.equal(event, "tool_call");
      handler = callback;
    },
  };

  await dangerousCommandGuard(pi as never);
  assert.ok(handler);
  assert.deepEqual(await handler({ toolName: "bash", input: { command: "rm -rf /tmp/example" } }), {
    block: true,
    reason: "rm -rf is blocked for safety",
  });
  assert.equal(await handler({ toolName: "bash", input: { command: "echo 'rm -rf /tmp/example'" } }), undefined);
});
