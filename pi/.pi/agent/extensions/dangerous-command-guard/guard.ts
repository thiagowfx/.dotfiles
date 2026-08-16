import { createRequire } from "node:module";
import { posix as posixPath } from "node:path";

import { Language, type Node, Parser } from "web-tree-sitter";

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export interface BlockedCommand {
  command: string;
  reason: string;
}

type ShellWord = string | undefined;

const require = createRequire(import.meta.url);
let parserPromise: Promise<Parser> | undefined;

function getParser(): Promise<Parser> {
  parserPromise ??= (async () => {
    await Parser.init();
    const languagePath = require.resolve("tree-sitter-bash/tree-sitter-bash.wasm");
    const language = await Language.load(languagePath);
    return new Parser().setLanguage(language);
  })();
  return parserPromise;
}

const SHELL_EXECUTABLES = new Set(["bash", "dash", "ksh", "sh", "zsh"]);
const basename = (path: string) => path.slice(path.lastIndexOf("/") + 1);

function decodeStaticWord(source: string): ShellWord {
  let result = "";
  let quote: "single" | "double" | undefined;

  for (let index = 0; index < source.length; index += 1) {
    const character = source[index];

    if (quote === "single") {
      if (character === "'") quote = undefined;
      else result += character;
      continue;
    }

    if (quote === "double") {
      if (character === '"') {
        quote = undefined;
        continue;
      }
      if (character === "$" || character === "`") return undefined;
      if (character === "\\") {
        const next = source[index + 1];
        if (next === undefined) return undefined;
        if ('$`"\\\n'.includes(next)) {
          if (next !== "\n") result += next;
          index += 1;
        } else {
          result += character;
        }
        continue;
      }
      result += character;
      continue;
    }

    if (character === "'") {
      quote = "single";
      continue;
    }
    if (character === '"') {
      quote = "double";
      continue;
    }
    if (character === "$" || character === "`" || "*?[".includes(character)) return undefined;
    if (character === "\\") {
      const next = source[index + 1];
      if (next === undefined) return undefined;
      if (next !== "\n") result += next;
      index += 1;
      continue;
    }
    result += character;
  }

  return quote === undefined ? result : undefined;
}

function hasShortOption(argument: string, option: string): boolean {
  return argument.startsWith("-") && !argument.startsWith("--") && argument.slice(1).includes(option);
}

function firstPositional(
  arguments_: ShellWord[],
  optionsWithValues: ReadonlySet<string> = new Set(),
): ShellWord {
  let skipValue = false;

  for (const argument of arguments_) {
    if (skipValue) {
      skipValue = false;
      continue;
    }
    if (argument === undefined) return undefined;
    if (argument === "--") continue;
    if (!argument.startsWith("-")) return argument;

    const option = argument.split("=", 1)[0];
    if (!argument.includes("=") && optionsWithValues.has(option)) skipValue = true;
  }

  return undefined;
}

function commandAfterOptions(arguments_: ShellWord[], optionsWithValues: ReadonlySet<string>): ShellWord[] {
  for (let index = 0; index < arguments_.length; index += 1) {
    const argument = arguments_[index];
    if (argument === undefined) return [undefined];
    if (argument === "--") return arguments_.slice(index + 1);
    if (!argument.startsWith("-")) return arguments_.slice(index);

    const option = argument.split("=", 1)[0];
    if (!argument.includes("=") && optionsWithValues.has(option)) index += 1;
  }

  return [];
}

function containsPreCommitCachePath(argument: string): boolean {
  const segments = argument.split("/");
  return segments.some((segment, index) => segment === ".cache" && segments[index + 1] === "pre-commit");
}

function isTmpPath(argument: string): boolean {
  if (!argument.startsWith("/")) return false;
  const normalized = posixPath.normalize(argument);
  return normalized === "/tmp" || normalized.startsWith("/tmp/");
}

function rmOperands(arguments_: ShellWord[]): ShellWord[] {
  const operands: ShellWord[] = [];
  let optionsEnded = false;

  for (const argument of arguments_) {
    if (optionsEnded || argument === undefined) {
      operands.push(argument);
      continue;
    }
    if (argument === "--") {
      optionsEnded = true;
      continue;
    }
    if (argument.startsWith("-") && argument !== "-") continue;
    operands.push(argument);
  }

  return operands;
}

function protectedGitRef(argument: string): boolean {
  return argument
    .split(":")
    .map((part) => part.replace(/^\+/, "").replace(/^refs\/heads\//, ""))
    .some((part) => part === "main" || part === "master");
}

function unwrapCommand(words: ShellWord[]): ShellWord[] {
  let current = words;

  while (current[0] !== undefined) {
    const executable = basename(current[0]);

    if (executable === "command") {
      if (current.some((argument) => argument === "-v" || argument === "-V")) return [];
      current = current.slice(1);
      while (current[0]?.startsWith("-")) current = current.slice(1);
      continue;
    }

    if (executable === "env") {
      current = current.slice(1);
      while (current.length > 0) {
        const argument = current[0];
        if (argument === undefined) return current;
        if (argument === "-u" || argument === "--unset" || argument === "-C" || argument === "--chdir") {
          current = current.slice(2);
          continue;
        }
        if (argument.startsWith("-") || argument.includes("=")) {
          current = current.slice(1);
          continue;
        }
        break;
      }
      continue;
    }

    if (executable === "sudo") {
      const optionsWithValues = new Set([
        "-a",
        "--auth-type",
        "-C",
        "--close-from",
        "-D",
        "--chdir",
        "-g",
        "--group",
        "-h",
        "--host",
        "-p",
        "--prompt",
        "-R",
        "--chroot",
        "-r",
        "--role",
        "-T",
        "--command-timeout",
        "-t",
        "--type",
        "-U",
        "--other-user",
        "-u",
        "--user",
      ]);

      current = current.slice(1);
      while (current.length > 0) {
        const option = current[0];
        if (option === "--") {
          current = current.slice(1);
          break;
        }
        if (option !== undefined && optionsWithValues.has(option)) {
          current = current.slice(2);
          continue;
        }
        if (option?.startsWith("-")) {
          current = current.slice(1);
          continue;
        }
        break;
      }
      continue;
    }

    break;
  }

  return current;
}

function inspectGit(arguments_: ShellWord[], command: string): BlockedCommand | undefined {
  const subcommand = firstPositional(arguments_, new Set(["-C", "-c", "--git-dir", "--work-tree", "--namespace"]));
  if (subcommand === undefined) {
    return { command, reason: "Dynamic git arguments cannot be inspected safely" };
  }

  const subcommandIndex = arguments_.indexOf(subcommand);
  const subcommandArguments = arguments_.slice(subcommandIndex + 1);
  const staticArguments = subcommandArguments.filter((argument): argument is string => argument !== undefined);
  const hasDynamicArguments = staticArguments.length !== subcommandArguments.length;

  if ((subcommand === "commit" || subcommand === "push") && staticArguments.includes("--no-verify")) {
    return { command, reason: "--no-verify is blocked - run the hooks" };
  }

  if (subcommand === "reset" && staticArguments.includes("--hard")) {
    return { command, reason: "git reset --hard is blocked - discards changes irreversibly" };
  }

  if (subcommand === "clean") {
    const dryRun = staticArguments.some((argument) => argument === "--dry-run" || hasShortOption(argument, "n"));
    const destructive = staticArguments.some(
      (argument) =>
        argument === "--force" ||
        argument === "--directories" ||
        argument === "--ignored" ||
        hasShortOption(argument, "f") ||
        hasShortOption(argument, "d") ||
        hasShortOption(argument, "x"),
    );
    if (destructive && !dryRun) {
      return { command, reason: "git clean is blocked - deletes untracked files irreversibly" };
    }
  }

  if (subcommand === "push") {
    const dryRun = staticArguments.some((argument) => argument === "--dry-run" || hasShortOption(argument, "n"));
    const force = staticArguments.some(
      (argument) =>
        argument === "--force" || argument.startsWith("--force-with-lease") || hasShortOption(argument, "f"),
    );
    const protectedBranch = staticArguments.some(protectedGitRef);
    if (force && protectedBranch && !dryRun) {
      return { command, reason: "Force-push to master/main is blocked" };
    }
  }

  if (hasDynamicArguments && ["clean", "push", "reset"].includes(subcommand)) {
    return { command, reason: `Dynamic git ${subcommand} arguments cannot be inspected safely` };
  }

  return undefined;
}

function inspectWords(words: ShellWord[], command: string, parser: Parser): BlockedCommand | undefined {
  if (words[0] !== undefined && basename(words[0]) === "env") {
    const envArguments = words.slice(1);
    const splitIndex = envArguments.findIndex(
      (argument) => argument === "-S" || argument === "--split-string" || argument?.startsWith("--split-string="),
    );
    if (splitIndex >= 0) {
      const option = envArguments[splitIndex];
      const splitCommand = option?.startsWith("--split-string=")
        ? option.slice("--split-string=".length)
        : envArguments[splitIndex + 1];
      const trailingStart = splitIndex + (option?.includes("=") ? 1 : 2);
      const trailing = envArguments.slice(trailingStart);
      if (splitCommand === undefined || trailing.some((argument) => argument === undefined)) {
        return { command, reason: "Dynamic env split-string command cannot be inspected safely" };
      }
      return inspectSource([splitCommand, ...trailing].join(" "), parser);
    }
  }

  const unwrapped = unwrapCommand(words);
  const executableWord = unwrapped[0];
  // Unknown executable names are not evidence of danger; let the shell resolve them.
  if (executableWord === undefined) return undefined;

  const executable = basename(executableWord);
  const arguments_ = unwrapped.slice(1);
  const staticArguments = arguments_.filter((argument): argument is string => argument !== undefined);
  const hasDynamicArguments = staticArguments.length !== arguments_.length;

  if (SHELL_EXECUTABLES.has(executable)) {
    const commandOptionIndex = staticArguments.findIndex(
      (argument) => argument === "-c" || (hasShortOption(argument, "c") && argument !== "--"),
    );
    if (commandOptionIndex >= 0) {
      const nestedCommand = staticArguments[commandOptionIndex + 1];
      if (nestedCommand === undefined) {
        return { command, reason: "Dynamic nested shell command cannot be inspected safely" };
      }
      return inspectSource(nestedCommand, parser);
    }
  }

  if (executable === "eval") {
    if (hasDynamicArguments) return { command, reason: "Dynamic eval arguments cannot be inspected safely" };
    return inspectSource(staticArguments.join(" "), parser);
  }

  if (executable === "nohup") {
    return inspectWords(commandAfterOptions(arguments_, new Set()), command, parser);
  }

  if (executable === "nice") {
    return inspectWords(commandAfterOptions(arguments_, new Set(["-n", "--adjustment"])), command, parser);
  }

  if (executable === "xargs") {
    const nested = commandAfterOptions(
      arguments_,
      new Set([
        "-a",
        "--arg-file",
        "-d",
        "--delimiter",
        "-E",
        "--eof",
        "-I",
        "--replace",
        "-L",
        "--max-lines",
        "-n",
        "--max-args",
        "-P",
        "--max-procs",
        "-s",
        "--max-chars",
      ]),
    );
    return nested.length > 0 ? inspectWords(nested, command, parser) : undefined;
  }

  if (executable === "watch" || executable === "parallel") {
    const nested = commandAfterOptions(
      arguments_,
      executable === "watch"
        ? new Set(["-n", "--interval", "--chdir"])
        : new Set(["-a", "--arg-file", "-j", "--jobs", "-S", "--sshlogin", "--workdir"]),
    );
    const inputSeparator = nested.findIndex((argument) => argument === ":::");
    const commandWords = nested.slice(0, inputSeparator < 0 ? nested.length : inputSeparator);
    if (commandWords.some((argument) => argument === undefined)) {
      return { command, reason: `Dynamic ${executable} command cannot be inspected safely` };
    }
    return commandWords.length > 0 ? inspectSource(commandWords.join(" "), parser) : undefined;
  }

  if (executable === "find") {
    for (let index = 0; index < arguments_.length; index += 1) {
      if (arguments_[index] !== "-exec" && arguments_[index] !== "-execdir") continue;
      const end = arguments_.findIndex((argument, offset) => offset > index && (argument === ";" || argument === "+"));
      const nested = arguments_.slice(index + 1, end < 0 ? undefined : end);
      const blocked = inspectWords(nested, nested.filter(Boolean).join(" "), parser);
      if (blocked) return blocked;
    }
  }

  if (executable === "rm") {
    const recursive = staticArguments.some(
      (argument) => argument === "--recursive" || hasShortOption(argument, "r") || hasShortOption(argument, "R"),
    );
    const force = staticArguments.some(
      (argument) => argument === "--force" || hasShortOption(argument, "f"),
    );
    if (recursive && force) {
      const operands = rmOperands(arguments_);
      const allTmp = operands.length > 0 && operands.every((operand) => operand !== undefined && isTmpPath(operand));
      if (!allTmp) return { command, reason: "rm -rf is blocked for safety" };
    }
    if (staticArguments.some(containsPreCommitCachePath)) {
      return { command, reason: "Deleting pre-commit cache is blocked for safety" };
    }
    if (hasDynamicArguments) {
      return { command, reason: "Dynamic arguments to rm cannot be inspected safely" };
    }
  }

  if (["terraform", "tofu", "terragrunt"].includes(executable)) {
    if (staticArguments.includes("--auto-approve")) {
      return { command, reason: "--auto-approve is blocked - manual confirmation required" };
    }
    const subcommand = firstPositional(arguments_);
    if (subcommand === "apply") {
      return { command, reason: "terraform apply is blocked - use terraform plan first" };
    }
    if (subcommand === "destroy") return { command, reason: "terraform destroy is blocked for safety" };
    if (hasDynamicArguments) {
      return { command, reason: `Dynamic ${executable} arguments cannot be inspected safely` };
    }
  }

  if (executable === "just") {
    const recipe = firstPositional(arguments_, new Set(["-f", "--justfile", "-d", "--working-directory", "--shell"]));
    if (recipe === "apply") return { command, reason: "just apply is blocked - use just plan first" };
    if (recipe === "destroy") return { command, reason: "just destroy is blocked for safety" };
    if (hasDynamicArguments) {
      return { command, reason: "Dynamic just arguments cannot be inspected safely" };
    }
  }

  if (executable === "git") return inspectGit(arguments_, command);

  return undefined;
}

function commandWords(node: Node): ShellWord[] | undefined {
  const name = node.childForFieldName("name");
  if (!name) return undefined;
  return [
    decodeStaticWord(name.text),
    ...node.childrenForFieldName("argument").map((argument) => decodeStaticWord(argument.text)),
  ];
}

function heredocBodies(node: Node): Node[] {
  const bodies = node.type === "heredoc_body" ? [node] : [];
  for (const child of node.namedChildren) bodies.push(...heredocBodies(child));
  return bodies;
}

function inspectTree(node: Node, parser: Parser): BlockedCommand | undefined {
  if (node.type === "redirected_statement") {
    const body = node.childForFieldName("body");
    const words = body?.type === "command" ? commandWords(body) : undefined;
    if (words) {
      const executableWord = unwrapCommand(words)[0];
      if (executableWord !== undefined && SHELL_EXECUTABLES.has(basename(executableWord))) {
        for (const heredoc of heredocBodies(node)) {
          const blocked = inspectSource(heredoc.text, parser);
          if (blocked) return blocked;
        }
      }
    }
  }

  if (node.type === "command") {
    const words = commandWords(node);
    if (words) {
      const blocked = inspectWords(words, node.text, parser);
      if (blocked) return blocked;
    }
  }

  for (const child of node.namedChildren) {
    const blocked = inspectTree(child, parser);
    if (blocked) return blocked;
  }

  return undefined;
}

// tree-sitter-bash misparses "<<'EOF' 2>&1 | cmd" (a heredoc redirect followed by another
// redirect, then piped): it emits an ERROR node instead of accepting valid bash syntax. Moving
// the trailing fd-redirect ahead of the heredoc operator on that line is semantically
// equivalent (redirect order doesn't matter here) and resolves the ambiguity, so retry once
// with that reordering before giving up and blocking.
const HEREDOC_TRAILING_REDIRECT = /(<<-?\s*(?:'[^']*'|"[^"]*"|[^\s|&;<>]+))((?:\s+\d*(?:>&\d+|<&\d+|>>?\S+|<\S+))+)(\s*\|)/;

function reorderHeredocTrailingRedirects(source: string): string | undefined {
  let changed = false;
  const lines = source.split("\n").map((line) => {
    const match = HEREDOC_TRAILING_REDIRECT.exec(line);
    if (!match) return line;
    changed = true;
    const [whole, heredocPart, redirectPart, pipePart] = match;
    return (
      line.slice(0, match.index) +
      redirectPart.trim() +
      " " +
      heredocPart +
      pipePart +
      line.slice(match.index + whole.length)
    );
  });
  return changed ? lines.join("\n") : undefined;
}

function inspectSource(source: string, parser: Parser): BlockedCommand | undefined {
  const tree = parser.parse(source);
  if (!tree) return { command: source, reason: "Shell command could not be parsed safely" };

  try {
    if (tree.rootNode.hasError) {
      const reordered = reorderHeredocTrailingRedirects(source);
      if (reordered !== undefined) {
        const recovered = parser.parse(reordered);
        try {
          if (recovered && !recovered.rootNode.hasError) return inspectTree(recovered.rootNode, parser);
        } finally {
          recovered?.delete();
        }
      }
      return { command: source, reason: "Shell command could not be parsed safely" };
    }
    return inspectTree(tree.rootNode, parser);
  } finally {
    tree.delete();
  }
}

export async function findBlockedCommand(source: string): Promise<BlockedCommand | undefined> {
  return inspectSource(source, await getParser());
}

export default async function dangerousCommandGuard(pi: ExtensionAPI) {
  await getParser();

  pi.on("tool_call", async (event, ctx) => {
    if (event.toolName !== "bash") return;

    const command = event.input.command;
    if (typeof command !== "string") return;

    const blocked = await findBlockedCommand(command);
    if (!blocked) return;
    if (!ctx.hasUI) return { block: true, reason: blocked.reason };

    const allowed = await ctx.ui.confirm(
      "Allow dangerous command?",
      `${blocked.command}\n\n${blocked.reason}`,
    );
    if (!allowed) return { block: true, reason: "Blocked by user" };
  });
}
