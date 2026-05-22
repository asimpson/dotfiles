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

type GcxParams = {
	args?: string[];
	cwd?: string;
	timeoutMs?: number;
	agentMode?: boolean;
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

type GcxDetails = {
	command: string;
	args: string[];
	exitCode: number;
	killed: boolean;
	cwd: string;
	stdout: Omit<StreamSummary, "text">;
	stderr: Omit<StreamSummary, "text">;
};

const TOOL_NAME = "gcx";
const DEFAULT_TIMEOUT_MS = 60_000;
const MAX_TIMEOUT_MS = 10 * 60_000;

const GcxToolParameters = Type.Object({
	args: Type.Optional(
		Type.Array(Type.String(), {
			description:
				"Arguments to pass to gcx, excluding the leading 'gcx'. Example: ['resources', 'get', 'dashboards', '-o', 'json'].",
		}),
	),
	cwd: Type.Optional(Type.String({ description: "Working directory for commands that read local files (defaults to pi cwd)." })),
	timeoutMs: Type.Optional(
		Type.Number({ description: `Timeout in milliseconds (default ${DEFAULT_TIMEOUT_MS}, max ${MAX_TIMEOUT_MS}).` }),
	),
	agentMode: Type.Optional(Type.Boolean({ description: "Prepend --agent for no-color, agent-friendly output (default true)." })),
});

function clampTimeout(timeoutMs: number | undefined): number {
	return Math.min(Math.max(timeoutMs ?? DEFAULT_TIMEOUT_MS, 1), MAX_TIMEOUT_MS);
}

function formatCommand(args: string[]): string {
	return [TOOL_NAME, ...args].join(" ");
}

async function saveFullOutput(streamName: StreamName, output: string): Promise<string> {
	const tempDir = await mkdtemp(join(tmpdir(), "pi-gcx-"));
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

export default function gcxExtension(pi: ExtensionAPI) {
	pi.registerTool({
		name: TOOL_NAME,
		label: TOOL_NAME,
		description:
			"Run the gcx CLI for Grafana Cloud operations. Pass args as an array excluding the leading 'gcx'. Output is truncated to " +
			`${DEFAULT_MAX_LINES} lines or ${formatSize(DEFAULT_MAX_BYTES)} per stream; truncated full output is saved to /tmp.`,
		promptSnippet: "Run gcx CLI commands for Grafana Cloud discovery, queries, and managed resource operations",
		promptGuidelines: [
			"Use the gcx tool for Grafana Cloud tasks: dashboards, datasources, alerting, SLOs, Synthetic Monitoring, IRM/OnCall, k6, Fleet, logs, metrics, traces, profiles, and resources.",
			"Before using gcx for an unfamiliar operation, discover commands with gcx args [\"help-tree\", \"--depth\", \"1\", \"-o\", \"text\"] and then drill into a group with [\"help-tree\", \"<group>\", \"-o\", \"text\"].",
			"Prefer dedicated gcx commands over gcx api; only use gcx api when no dedicated command exists.",
			"For programmatic reads through gcx, prefer structured output such as -o json and field selection with --json when available.",
			"For mutations through gcx, verify context first, read current state, use --dry-run when available, apply, then verify by reading back the resource.",
			"Never use gcx with --log-http-payload or gcx config view --raw.",
		],
		parameters: GcxToolParameters,
		async execute(_toolCallId, params: GcxParams, signal, onUpdate, ctx) {
			const args = params.agentMode === false ? [...(params.args ?? [])] : ["--agent", ...(params.args ?? [])];
			const timeout = clampTimeout(params.timeoutMs);
			const cwd = params.cwd ?? ctx.cwd;
			const command = formatCommand(args);

			onUpdate?.({ content: [{ type: "text", text: `Running ${command}` }], details: { command, args, cwd } });

			const result = await pi.exec(TOOL_NAME, args, { signal, timeout, cwd });
			const [stdout, stderr] = await Promise.all([
				summarizeStream("stdout", result.stdout),
				summarizeStream("stderr", result.stderr),
			]);

			const sections = [`$ ${command}`, `exit code: ${result.code}${result.killed ? " (killed)" : ""}`];
			if (stdout.text) sections.push(`\nstdout:\n${stdout.text.trimEnd()}`);
			if (stderr.text) sections.push(`\nstderr:\n${stderr.text.trimEnd()}`);

			const details: GcxDetails = {
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
