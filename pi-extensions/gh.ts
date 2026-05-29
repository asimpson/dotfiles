import { mkdtemp, readFile, readdir, writeFile } from "node:fs/promises";
import { homedir, tmpdir } from "node:os";
import { join } from "node:path";
import type { ExecResult, ExtensionAPI } from "@earendil-works/pi-coding-agent";
import {
	DEFAULT_MAX_BYTES,
	DEFAULT_MAX_LINES,
	formatSize,
	truncateHead,
	truncateTail,
	withFileMutationQueue,
} from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";

type GhParams = {
	args?: string[];
	cwd?: string;
	timeoutMs?: number;
};

type ReadOnlyToken = {
	token: string;
	source: string;
};

type TokenAttempt = {
	source: string;
	exitCode: number;
	killed: boolean;
	notFound: boolean;
};

type StreamName = "stdout" | "stderr";

type StreamSummary = {
	text: string;
	truncated: boolean;
	fullOutputPath?: string;
	outputLines?: number;
	totalLines?: number;
	outputBytes?: number;
	totalBytes?: number;
};

type GhDetails = {
	command: string;
	args: string[];
	exitCode: number;
	killed: boolean;
	cwd: string;
	tokenSource: string;
	tokenAttempts: TokenAttempt[];
	stdout: Omit<StreamSummary, "text">;
	stderr: Omit<StreamSummary, "text">;
};

const TOOL_NAME = "gh";
const TOKEN_DIR = "~/.ro-tokens";
const DEFAULT_TIMEOUT_MS = 60_000;
const MAX_TIMEOUT_MS = 10 * 60_000;

const GhToolParameters = Type.Object({
	args: Type.Optional(
		Type.Array(Type.String(), {
			description:
				"Arguments to pass to gh, excluding the leading 'gh'. Example: ['pr', 'view', '123', '--json', 'title,state,url'].",
		}),
	),
	cwd: Type.Optional(Type.String({ description: "Working directory for commands that read local repo context (defaults to pi cwd)." })),
	timeoutMs: Type.Optional(
		Type.Number({ description: `Timeout in milliseconds (default ${DEFAULT_TIMEOUT_MS}, max ${MAX_TIMEOUT_MS}).` }),
	),
});

let ghEnvLock: Promise<void> = Promise.resolve();

function clampTimeout(timeoutMs: number | undefined): number {
	return Math.min(Math.max(timeoutMs ?? DEFAULT_TIMEOUT_MS, 1), MAX_TIMEOUT_MS);
}

function expandHome(path: string): string {
	if (path === "~") return homedir();
	if (path.startsWith("~/")) return join(homedir(), path.slice(2));
	return path;
}

function errorMessage(error: unknown): string {
	return error instanceof Error ? error.message : String(error);
}

function errorCode(error: unknown): string | undefined {
	if (!error || typeof error !== "object") return undefined;
	const code = (error as { code?: unknown }).code;
	return typeof code === "string" ? code : undefined;
}

function parseTokenContent(content: string): string | undefined {
	return content
		.split(/\r?\n/)
		.map((line) => line.trim())
		.find((line) => line.length > 0 && !line.startsWith("#"));
}

async function readTokenContent(path: string): Promise<string> {
	try {
		return await readFile(path, "utf8");
	} catch (error) {
		throw new Error(`Could not read read-only GitHub token file ${path}: ${errorMessage(error)}`);
	}
}

async function getDirectoryTokens(): Promise<ReadOnlyToken[]> {
	const tokenDir = expandHome(TOKEN_DIR);
	let entries;

	try {
		entries = await readdir(tokenDir, { withFileTypes: true });
	} catch (error) {
		if (errorCode(error) === "ENOENT") return [];
		throw new Error(`Could not read read-only GitHub token directory ${tokenDir}: ${errorMessage(error)}`);
	}

	const tokens: ReadOnlyToken[] = [];
	const seenTokens = new Set<string>();
	const tokenFiles = entries
		.filter((entry) => entry.isFile() || entry.isSymbolicLink())
		.sort((a, b) => a.name.localeCompare(b.name));

	for (const entry of tokenFiles) {
		const source = join(tokenDir, entry.name);
		const token = parseTokenContent(await readTokenContent(source));
		if (!token || seenTokens.has(token)) continue;

		seenTokens.add(token);
		tokens.push({ token, source });
	}

	return tokens;
}

async function getReadOnlyTokens(): Promise<ReadOnlyToken[]> {
	const directoryTokens = await getDirectoryTokens();
	if (directoryTokens.length > 0) return directoryTokens;

	throw new Error(`No read-only GitHub tokens found. Add one token per file under ${expandHome(TOKEN_DIR)}.`);
}

function formatCommand(args: string[]): string {
	return [TOOL_NAME, ...args].join(" ");
}

function redactTokens(output: string, tokens: ReadOnlyToken[]): string {
	let redacted = output;
	for (const token of [...tokens].sort((a, b) => b.token.length - a.token.length)) {
		if (token.token.length === 0) continue;
		redacted = redacted.split(token.token).join("[redacted-gh-token]");
	}
	return redacted;
}

async function saveFullOutput(streamName: StreamName, output: string): Promise<string> {
	const tempDir = await mkdtemp(join(tmpdir(), "pi-gh-"));
	const tempFile = join(tempDir, `${streamName}.txt`);
	await withFileMutationQueue(tempFile, async () => writeFile(tempFile, output, "utf8"));
	return tempFile;
}

async function summarizeStream(streamName: StreamName, output: string): Promise<StreamSummary> {
	if (!output) return { text: "", truncated: false };

	const truncation = streamName === "stderr"
		? truncateTail(output, { maxLines: DEFAULT_MAX_LINES, maxBytes: DEFAULT_MAX_BYTES })
		: truncateHead(output, { maxLines: DEFAULT_MAX_LINES, maxBytes: DEFAULT_MAX_BYTES });

	if (!truncation.truncated) {
		return {
			text: truncation.content,
			truncated: false,
			outputLines: truncation.outputLines,
			totalLines: truncation.totalLines,
			outputBytes: truncation.outputBytes,
			totalBytes: truncation.totalBytes,
		};
	}

	const fullOutputPath = await saveFullOutput(streamName, output);
	const notice = `\n\n[${streamName} truncated: showing ${truncation.outputLines} of ${truncation.totalLines} lines ` +
		`(${formatSize(truncation.outputBytes)} of ${formatSize(truncation.totalBytes)}). Full output saved to: ${fullOutputPath}]`;

	return {
		text: truncation.content + notice,
		truncated: true,
		fullOutputPath,
		outputLines: truncation.outputLines,
		totalLines: truncation.totalLines,
		outputBytes: truncation.outputBytes,
		totalBytes: truncation.totalBytes,
	};
}

function streamDetails(summary: StreamSummary): Omit<StreamSummary, "text"> {
	const { text: _text, ...rest } = summary;
	return rest;
}

async function withGhEnvironment<T>(readOnlyToken: ReadOnlyToken, fn: () => Promise<T>): Promise<T> {
	let release!: () => void;
	const previous = ghEnvLock;
	ghEnvLock = new Promise<void>((resolve) => {
		release = resolve;
	});

	await previous;

	const envPatch: Record<string, string> = {
		GH_TOKEN: readOnlyToken.token,
		GH_PROMPT_DISABLED: "1",
		GH_NO_UPDATE_NOTIFIER: "1",
		NO_COLOR: "1",
		CLICOLOR: "0",
		PAGER: "cat",
		GH_PAGER: "cat",
	};
	const oldValues = new Map<string, string | undefined>();

	for (const [key, value] of Object.entries(envPatch)) {
		oldValues.set(key, process.env[key]);
		process.env[key] = value;
	}

	try {
		return await fn();
	} finally {
		for (const [key, value] of oldValues) {
			if (value === undefined) {
				delete process.env[key];
			} else {
				process.env[key] = value;
			}
		}
		release();
	}
}

function isRetryableNotFound(result: ExecResult): boolean {
	if (result.code === 0) return false;

	const output = `${result.stdout}\n${result.stderr}`;
	return (
		/\bHTTP\s+404\b/i.test(output) ||
		/"status"\s*:\s*"?404"?/i.test(output) ||
		/\bstatus:\s*404\b/i.test(output) ||
		/\bNot Found \(HTTP 404\)/i.test(output) ||
		/\bCould not resolve to a Repository with the name\b/i.test(output) ||
		/\brepository not found\b/i.test(output)
	);
}

function assertSafeArgs(args: string[]): void {
	const [command, subcommand, ...rest] = args;
	if (command !== "auth") return;

	const authAction = subcommand ?? "";
	const showsToken = rest.some((arg) => arg === "--show-token" || arg.startsWith("--show-token="));
	if (authAction === "status" && !showsToken) return;

	throw new Error(
		"gh auth commands are blocked by the gh tool so read-only tokens cannot be printed or modified. " +
			"Use gh args ['api', 'user', '-q', '.login'] to check the authenticated user.",
	);
}

function formatAttempt(attempt: TokenAttempt): string {
	return `${attempt.source}: exit ${attempt.exitCode}${attempt.killed ? " (killed)" : ""}${attempt.notFound ? " (404/not-found)" : ""}`;
}

export default function ghExtension(pi: ExtensionAPI) {
	pi.registerTool({
		name: TOOL_NAME,
		label: TOOL_NAME,
		description:
			"Run the GitHub CLI using read-only tokens from ~/.ro-tokens, one token per file. Pass args as an array excluding the leading 'gh'. " +
			"Avoid taking write actions on public repos, even if GitHub would allow them. " +
			"On GitHub 404/not-found responses, the tool retries with the next token. Output is truncated to " +
			`${DEFAULT_MAX_LINES} lines or ${formatSize(DEFAULT_MAX_BYTES)} per stream; truncated full output is saved to /tmp.`,
		promptSnippet: "Run GitHub CLI read-only queries with gh using read-only tokens from ~/.ro-tokens",
		promptGuidelines: [
			"Use the gh tool for GitHub read-only tasks instead of bashing out to gh directly: repositories, issues, pull requests, releases, Actions status, and GitHub API reads.",
			"The gh tool reads read-only GitHub tokens from ~/.ro-tokens, one token per file sorted by filename, and retries with the next token on GitHub 404/not-found responses.",
			"Treat gh access as read-only: do not use gh for create/edit/delete/comment/merge/upload/workflow-run operations or local clone/checkout/sync operations.",
			"Pass gh arguments as args excluding the leading gh, for example gh args [\"pr\", \"view\", \"123\", \"--json\", \"title,state,url\"].",
			"For programmatic reads through gh, prefer structured output such as --json with --jq, or gh api with --method GET and -q/--jq filters.",
			"For unfamiliar gh operations, discover usage with gh args [\"help\"] or gh args [\"<command>\", \"--help\"] before running the operation.",
			"Never use gh auth token or commands that print credentials; gh auth commands are blocked except gh auth status without --show-token, and configured-token occurrences are redacted from output.",
		],
		parameters: GhToolParameters,
		async execute(_toolCallId, params: GhParams, signal, onUpdate, ctx) {
			const args = [...(params.args ?? [])];
			assertSafeArgs(args);

			const timeout = clampTimeout(params.timeoutMs);
			const cwd = params.cwd ?? ctx.cwd;
			const command = formatCommand(args);
			const readOnlyTokens = await getReadOnlyTokens();

			onUpdate?.({
				content: [{ type: "text", text: `Running ${command} with ${readOnlyTokens.length} read-only GitHub token(s)` }],
				details: { command, args, cwd, tokenSources: readOnlyTokens.map((token) => token.source) },
			});

			let result: ExecResult | undefined;
			let selectedToken: ReadOnlyToken | undefined;
			const attempts: TokenAttempt[] = [];

			for (const [index, token] of readOnlyTokens.entries()) {
				result = await withGhEnvironment(token, () => pi.exec(TOOL_NAME, args, { signal, timeout, cwd }));
				selectedToken = token;

				const notFound = isRetryableNotFound(result);
				const attempt: TokenAttempt = {
					source: token.source,
					exitCode: result.code,
					killed: result.killed,
					notFound,
				};
				attempts.push(attempt);

				if (result.code === 0 || !notFound || index === readOnlyTokens.length - 1) break;

				onUpdate?.({
					content: [
						{
							type: "text",
							text: `${command} returned 404/not-found with ${token.source}; trying next token (${index + 2}/${readOnlyTokens.length})`,
						},
					],
					details: { command, args, cwd, attempts: [...attempts], nextTokenSource: readOnlyTokens[index + 1]?.source },
				});
			}

			if (!result || !selectedToken) throw new Error(`No read-only GitHub tokens available for ${command}`);

			const redactedStdout = redactTokens(result.stdout, readOnlyTokens);
			const redactedStderr = redactTokens(result.stderr, readOnlyTokens);
			const [stdout, stderr] = await Promise.all([
				summarizeStream("stdout", redactedStdout),
				summarizeStream("stderr", redactedStderr),
			]);

			const sections = [`$ ${command}`, `exit code: ${result.code}${result.killed ? " (killed)" : ""}`];
			if (attempts.length > 1) sections.push(`token attempts: ${attempts.map(formatAttempt).join("; ")}`);
			if (stdout.text) sections.push(`\nstdout:\n${stdout.text.trimEnd()}`);
			if (stderr.text) sections.push(`\nstderr:\n${stderr.text.trimEnd()}`);

			const details: GhDetails = {
				command,
				args,
				exitCode: result.code,
				killed: result.killed,
				cwd,
				tokenSource: selectedToken.source,
				tokenAttempts: attempts,
				stdout: streamDetails(stdout),
				stderr: streamDetails(stderr),
			};

			return { content: [{ type: "text", text: sections.join("\n") }], details };
		},
	});
}
