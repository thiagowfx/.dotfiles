import { constants } from "node:fs";
import { access } from "node:fs/promises";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

const extensionDirectory = dirname(fileURLToPath(import.meta.url));
const dependencyFiles = [
  join(extensionDirectory, "node_modules", "tree-sitter-bash", "tree-sitter-bash.wasm"),
  join(extensionDirectory, "node_modules", "web-tree-sitter", "package.json"),
];

type DependenciesAvailable = () => Promise<boolean>;

async function localDependenciesAvailable(): Promise<boolean> {
  try {
    await Promise.all(dependencyFiles.map((path) => access(path, constants.R_OK)));
    return true;
  } catch {
    return false;
  }
}

export async function ensureDependencies(
  pi: ExtensionAPI,
  dependenciesAvailable: DependenciesAvailable = localDependenciesAvailable,
): Promise<void> {
  if (await dependenciesAvailable()) return;

  const result = await pi.exec("npm", ["ci", "--ignore-scripts"], {
    cwd: extensionDirectory,
    timeout: 120_000,
  });
  if (result.code !== 0) {
    const details = result.stderr.trim() || result.stdout.trim() || `npm exited with code ${result.code}`;
    throw new Error(`Failed to install dangerous command guard dependencies: ${details}`);
  }
  if (!(await dependenciesAvailable())) {
    throw new Error("Failed to install dangerous command guard dependencies: required files are still missing");
  }
}

export async function bootstrapDangerousCommandGuard(
  pi: ExtensionAPI,
  dependenciesAvailable: DependenciesAvailable = localDependenciesAvailable,
): Promise<void> {
  await ensureDependencies(pi, dependenciesAvailable);
  const { default: dangerousCommandGuard } = await import("./guard.ts");
  await dangerousCommandGuard(pi);
}

export default bootstrapDangerousCommandGuard;
