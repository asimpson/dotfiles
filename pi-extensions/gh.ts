import { mkdtemp, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
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
	stdout: Omit<StreamSummary, "text">;
	stderr: Omit<StreamSummary, "text">;
};

const TOOL_NAME = "gh";
const DEFAULT_TIMEOUT_MS = 60_000;
const MAX_TIMEOUT_MS = 10 * 60_000;

// Only these built-in gh commands may be run. No aliases, extensions, downloads,
// local checkouts, auth/config changes, or commands that mutate GitHub state.
const READ_COMMANDS: Record<string, ReadonlySet<string>> = {
	gist: new Set(["list", "view"]),
	issue: new Set(["list", "status", "view"]),
	label: new Set(["list"]),
	org: new Set(["list"]),
	pr: new Set(["checks", "diff", "list", "status", "view"]),
	project: new Set(["field-list", "item-list", "list", "view"]),
	release: new Set(["list", "view"]),
	repo: new Set(["list", "read-dir", "read-file", "view"]),
	ruleset: new Set(["check", "list", "view"]),
	run: new Set(["list", "view", "watch"]),
	search: new Set(["code", "commits", "issues", "prs", "repos"]),
	workflow: new Set(["list", "view"]),
};

// gh api silently changes its default method to POST when given fields or input.
// Accept only options that cannot change the request method, body, or destination.
const API_VALUE_FLAGS = new Set(["--cache", "--jq", "-q", "--preview", "-p", "--template", "-t"]);
const API_SWITCH_FLAGS = new Set(["--paginate", "--slurp", "--silent"]);
const HELP_TOPICS = new Set(["accessibility", "actions", "environment", "exit-codes", "formatting", "reference", "telemetry"]);

const GhToolParameters = Type.Object({
	args: Type.Optional(
		Type.Array(Type.String(), {
			description:
				"Allowlisted read-only gh command arguments (without 'gh'), e.g. ['pr', 'view', '123', '--json', 'title,state,url'].",
		}),
	),
	cwd: Type.Optional(Type.String({ description: "Working directory for commands that read local repo context (defaults to pi cwd)." })),
	timeoutMs: Type.Optional(
		Type.Number({ description: `Timeout in milliseconds (default ${DEFAULT_TIMEOUT_MS}, max ${MAX_TIMEOUT_MS}).` }),
	),
});

function clampTimeout(timeoutMs: number | undefined): number {
	return Math.min(Math.max(timeoutMs ?? DEFAULT_TIMEOUT_MS, 1), MAX_TIMEOUT_MS);
}

function formatCommand(args: string[]): string {
	return [TOOL_NAME, ...args].join(" ");
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

function assertSafeApiArgs(args: string[]): void {
	if (args.length === 1 && args[0] === "--help") return;

	let endpoint: string | undefined;
	for (let i = 0; i < args.length; i++) {
		const arg = args[i];
		const [flag, inlineValue] = arg.startsWith("--") ? arg.split(/=(.*)/s, 2) : [arg, undefined];

		if (flag === "--method" || flag === "-X") {
			const method = inlineValue ?? args[++i];
			if (method !== "GET") throw new Error("gh api only permits --method GET (or -X GET)");
		} else if (API_VALUE_FLAGS.has(flag)) {
			if (!(inlineValue ?? args[++i])) throw new Error(`gh api requires a value for ${flag}`);
		} else if (API_SWITCH_FLAGS.has(arg)) {
			continue;
		} else if (!arg.startsWith("-") && !endpoint) {
			endpoint = arg;
		} else {
			throw new Error(`gh api argument not allowed: ${arg}`);
		}
	}

	// Restrict to relative REST paths. GraphQL is excluded: its queries use POST
	// and may contain mutations, even when described as read-only.
	if (!endpoint || !/^\/?[a-zA-Z0-9{][a-zA-Z0-9{}_.\-/?&=%+,:]*$/.test(endpoint) ||
		endpoint.includes("..") || endpoint.includes("//") || endpoint.split("?")[0].replace(/^\//, "") === "graphql") {
		throw new Error("gh api requires a relative REST endpoint (not graphql or a URL)");
	}
}

function readSubcommands(group: string | undefined): ReadonlySet<string> | undefined {
	return group && Object.hasOwn(READ_COMMANDS, group) ? READ_COMMANDS[group] : undefined;
}

function assertSafeArgs(args: string[]): void {
	const [command, subcommand, ...rest] = args;
	if (args.length === 1 && command === "--version") return;
	if (command === "help" && (args.length === 1 ||
		(args.length === 2 && (HELP_TOPICS.has(subcommand) || subcommand === "api" || subcommand === "status" || readSubcommands(subcommand))) ||
		(args.length === 3 && readSubcommands(subcommand)?.has(rest[0])))) return;
	if (command === "api") return assertSafeApiArgs(args.slice(1));

	const allowed = readSubcommands(command);
	if (allowed && subcommand === "--help" && rest.length === 0) return;
	if (command !== "status" && !allowed?.has(subcommand)) {
		throw new Error(`gh command not allowed: ${formatCommand(args)}. Use gh help or a listed read-only command.`);
	}
	// Even read-only commands may offer flags that launch a browser or write local files.
	const options = command === "status" ? args.slice(1) : rest;
	if (options.some((arg) => ["--web", "-w", "--output", "-O"].some((flag) => arg === flag || arg.startsWith(`${flag}=`)))) {
		throw new Error(`gh command option not allowed: ${formatCommand(args)}`);
	}
}

export default function ghExtension(pi: ExtensionAPI) {
	pi.registerTool({
		name: TOOL_NAME,
		label: TOOL_NAME,
		description:
			"Run allowlisted read-only GitHub CLI commands using gh's existing login (e.g. OAuth). " +
			"Pass args without the leading 'gh'. Mutating commands and gh api methods other than GET are blocked. " +
			`Output is truncated to ${DEFAULT_MAX_LINES} lines or ${formatSize(DEFAULT_MAX_BYTES)} per stream; truncated full output is saved to /tmp.`,
		promptSnippet: "Run allowlisted read-only GitHub CLI queries with your existing gh login",
		promptGuidelines: [
			"Use the gh tool for GitHub reads: repo, pr, issue, release, run, workflow, search, and gh api. Mutating commands are blocked.",
			"The gh tool uses gh's existing login (e.g. OAuth), not ~/.ro-tokens. Authenticate outside the tool with gh auth login; GH_TOKEN and GITHUB_TOKEN must be unset.",
			"Pass gh arguments without the leading gh, e.g. gh args [\"pr\", \"view\", \"123\", \"--json\", \"title,state,url\"].",
			"For programmatic reads, prefer --json and --jq, or gh api with a relative REST endpoint, GET, and -q/--jq. GraphQL, request fields and body options are blocked.",
			"Discover supported commands with gh args [\"help\"] or gh args [\"pr\", \"--help\"]. Only allowlisted read-only subcommands execute.",
			"Never request credentials with gh auth token; the gh tool blocks all auth commands. Do not use shell commands to bypass its read-only policy.",
		],
		annotations: { readOnlyHint: true, destructiveHint: false },
		parameters: GhToolParameters,
		async execute(_toolCallId, params: GhParams, signal, onUpdate, ctx) {
			const args = [...(params.args ?? [])];
			assertSafeArgs(args);
			// gh prioritizes these environment variables over its OAuth login.
			if (["GH_TOKEN", "GITHUB_TOKEN", "GH_ENTERPRISE_TOKEN", "GITHUB_ENTERPRISE_TOKEN"].some((name) => process.env[name])) {
				throw new Error("Unset GH_TOKEN/GITHUB_TOKEN (and enterprise equivalents) to use the gh OAuth login.");
			}

			const timeout = clampTimeout(params.timeoutMs);
			const cwd = params.cwd ?? ctx.cwd;
			const command = formatCommand(args);
			onUpdate?.({
				content: [{ type: "text", text: `Running ${command}` }],
				details: { command, args, cwd },
			});

			const result = await pi.exec(TOOL_NAME, args, { signal, timeout, cwd });
			const [stdout, stderr] = await Promise.all([
				summarizeStream("stdout", result.stdout),
				summarizeStream("stderr", result.stderr),
			]);

			const sections = [`$ ${command}`, `exit code: ${result.code}${result.killed ? " (killed)" : ""}`];
			if (stdout.text) sections.push(`\nstdout:\n${stdout.text.trimEnd()}`);
			if (stderr.text) sections.push(`\nstderr:\n${stderr.text.trimEnd()}`);

			const details: GhDetails = {
				command,
				args,
				exitCode: result.code,
				killed: result.killed,
				cwd,
				stdout: streamDetails(stdout),
				stderr: streamDetails(stderr),
			};

			return { content: [{ type: "text", text: sections.join("\n") }], details };
		},
	});
}
