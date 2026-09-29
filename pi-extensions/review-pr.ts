/**
 * /review-pr - PR review helper
 *
 * Prompts for a language (fuzzy-select from a common list, or type your own)
 * and a PR URL, then fills the editor with a review prompt so the user can
 * review/tweak before submitting.
 */

import { readFile, readdir } from "node:fs/promises";
import { homedir } from "node:os";
import { join } from "node:path";
import type { ExecResult, ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { DynamicBorder } from "@earendil-works/pi-coding-agent";
import {
	Container,
	fuzzyFilter,
	Key,
	matchesKey,
	type SelectItem,
	SelectList,
	Text,
} from "@earendil-works/pi-tui";

/**
 * Return the typed character for a raw key event if it's a printable text
 * character we want to append to the filter. Rejects escape sequences,
 * control chars, and DEL.
 */
function printableFromKey(data: string): string | undefined {
	if (!data || data.length === 0) return undefined;
	if (data.startsWith("\x1b")) return undefined; // escape sequence (arrows, etc.)
	for (let i = 0; i < data.length; i++) {
		const code = data.charCodeAt(i);
		if (code < 0x20 || code === 0x7f) return undefined;
	}
	return data;
}

const LANGUAGES = [
	"Bash",
	"C",
	"C#",
	"C++",
	"CSS",
	"Clojure",
	"Dart",
	"Elixir",
	"Erlang",
	"Go",
	"GraphQL",
	"HTML",
	"Haskell",
	"Java",
	"JavaScript",
	"Julia",
	"Kotlin",
	"Lua",
	"Markdown",
	"OCaml",
	"Objective-C",
	"PHP",
	"Perl",
	"PowerShell",
	"Python",
	"R",
	"Ruby",
	"Rust",
	"SQL",
	"Scala",
	"Shell",
	"Solidity",
	"Swift",
	"Terraform",
	"TypeScript",
	"YAML",
	"Zig",
].sort();

interface PrInfo {
	number: number;
	title: string;
}

type ReadOnlyToken = {
	token: string;
	source: string;
};

const TOKEN_DIR = "~/.ro-tokens";
let ghEnvLock: Promise<void> = Promise.resolve();

function expandHome(path: string): string {
	if (path === "~") return homedir();
	if (path.startsWith("~/")) return join(homedir(), path.slice(2));
	return path;
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

async function getReadOnlyTokens(): Promise<ReadOnlyToken[]> {
	const tokenDir = expandHome(TOKEN_DIR);
	let entries;
	try {
		entries = await readdir(tokenDir, { withFileTypes: true });
	} catch (error) {
		if (errorCode(error) === "ENOENT") return [];
		throw error;
	}

	const tokens: ReadOnlyToken[] = [];
	const seen = new Set<string>();
	const files = entries
		.filter((entry) => entry.isFile() || entry.isSymbolicLink())
		.sort((a, b) => a.name.localeCompare(b.name));

	for (const entry of files) {
		const source = join(tokenDir, entry.name);
		const token = parseTokenContent(await readFile(source, "utf8"));
		if (!token || seen.has(token)) continue;
		seen.add(token);
		tokens.push({ token, source });
	}
	return tokens;
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
			if (value === undefined) delete process.env[key];
			else process.env[key] = value;
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

async function fetchPrInfo(pi: ExtensionAPI, url: string): Promise<PrInfo | null> {
	const args = ["pr", "view", url, "--json", "number,title"];
	const runOnce = (token?: ReadOnlyToken): Promise<ExecResult> => {
		const exec = () => pi.exec("gh", args, { timeout: 15_000 });
		return token ? withGhEnvironment(token, exec) : exec();
	};

	let tokens: ReadOnlyToken[] = [];
	try {
		tokens = await getReadOnlyTokens();
	} catch {
		tokens = [];
	}

	// If no read-only tokens are configured, fall back to whatever gh finds in its own config.
	const attempts: (ReadOnlyToken | undefined)[] = tokens.length > 0 ? tokens : [undefined];

	for (let i = 0; i < attempts.length; i++) {
		let result: ExecResult;
		try {
			result = await runOnce(attempts[i]);
		} catch {
			return null;
		}

		if (result.code === 0) {
			try {
				const parsed = JSON.parse(result.stdout) as { number?: unknown; title?: unknown };
				if (typeof parsed.number !== "number" || typeof parsed.title !== "string") return null;
				const title = parsed.title.trim();
				if (!title) return null;
				return { number: parsed.number, title };
			} catch {
				return null;
			}
		}

		// Only rotate to the next token on 404/not-found style responses.
		if (!isRetryableNotFound(result) || i === attempts.length - 1) return null;
	}
	return null;
}

function buildPrompt(language: string, url: string): string {
	return `Review this PR ${url}. Evaluate it based on:

- Idiomatic ${language}
- Proper testing, not coverage per se but are there logical tests where it makes sense for the new or changed functionality
- Is the problem real and does the proposed change fix the problem
- Any security issues that arise from the changes

Don't make code changes or make any reply in the PR itself. Just report back here what your eval is.`;
}

async function pickLanguage(ctx: Parameters<Parameters<ExtensionAPI["registerCommand"]>[1]["handler"]>[1]): Promise<string | null> {
	return ctx.ui.custom<string | null>((tui, theme, _kb, done) => {
		let filter = "";
		let selectList = buildList(filter);
		const container = new Container();
		const filterLine = new Text("", 1, 0);
		const hintLine = new Text("", 1, 0);

		container.addChild(new DynamicBorder((s: string) => theme.fg("accent", s)));
		container.addChild(new Text(theme.fg("accent", theme.bold("Language")), 1, 0));
		container.addChild(filterLine);
		container.addChild(selectList);
		container.addChild(hintLine);
		container.addChild(new DynamicBorder((s: string) => theme.fg("accent", s)));

		refreshChrome();

		function buildList(f: string): SelectList {
			const items = itemsFor(f);
			const list = new SelectList(items, Math.min(Math.max(items.length, 1), 10), {
				selectedPrefix: (t) => theme.fg("accent", t),
				selectedText: (t) => theme.fg("accent", t),
				description: (t) => theme.fg("muted", t),
				scrollInfo: (t) => theme.fg("dim", t),
				noMatch: (t) => theme.fg("warning", t),
			});
			list.onSelect = (item) => done(item.value);
			list.onCancel = () => done(null);
			return list;
		}

		function itemsFor(f: string): SelectItem[] {
			const trimmed = f.trim();
			const base = trimmed ? fuzzyFilter(LANGUAGES, trimmed, (l) => l) : LANGUAGES;
			const items: SelectItem[] = base.map((name) => ({ value: name, label: name }));
			if (
				trimmed &&
				!LANGUAGES.some((l) => l.toLowerCase() === trimmed.toLowerCase())
			) {
				items.unshift({
					value: trimmed,
					label: `Use "${trimmed}"`,
					description: "custom entry",
				});
			}
			return items;
		}

		function rebuildList() {
			// Rebuild container children so the SelectList reflects the new filter.
			container.clear();
			selectList = buildList(filter);
			container.addChild(new DynamicBorder((s: string) => theme.fg("accent", s)));
			container.addChild(new Text(theme.fg("accent", theme.bold("Language")), 1, 0));
			container.addChild(filterLine);
			container.addChild(selectList);
			container.addChild(hintLine);
			container.addChild(new DynamicBorder((s: string) => theme.fg("accent", s)));
			refreshChrome();
		}

		function refreshChrome() {
			const shown = filter.length > 0 ? filter : theme.fg("dim", "(type to filter)");
			filterLine.setText(`${theme.fg("muted", "filter: ")}${shown}`);
			hintLine.setText(theme.fg("dim", "type to filter • ↑↓ navigate • enter select • esc cancel"));
		}

		function handleInput(data: string) {
			// Cancel
			if (matchesKey(data, Key.escape)) {
				done(null);
				return;
			}

			// Navigation + confirm - always delegate to the list
			if (
				matchesKey(data, Key.up) ||
				matchesKey(data, Key.down) ||
				matchesKey(data, Key.enter)
			) {
				selectList.handleInput(data);
				tui.requestRender();
				return;
			}

			// Backspace - shrink filter
			if (matchesKey(data, Key.backspace)) {
				if (filter.length > 0) {
					filter = filter.slice(0, -1);
					rebuildList();
					tui.requestRender();
				}
				return;
			}

			// Printable characters - append to filter
			const printable = printableFromKey(data);
			if (printable) {
				filter += printable;
				rebuildList();
				tui.requestRender();
				return;
			}
		}

		return {
			render: (w: number) => container.render(w),
			invalidate: () => container.invalidate(),
			handleInput,
		};
	});
}

export default function reviewPr(pi: ExtensionAPI) {
	pi.registerCommand("review-pr", {
		description: "Review a PR: fuzzy-pick a language and enter a PR URL, then loads a review prompt into the editor",
		handler: async (_args, ctx) => {
			if (!ctx.hasUI) {
				ctx.ui.notify("/review-pr requires an interactive UI", "error");
				return;
			}

			const url = await ctx.ui.input("PR URL", "https://github.com/owner/repo/pull/123");
			if (!url || !url.trim()) {
				ctx.ui.notify("Cancelled: no PR URL provided", "warning");
				return;
			}

			// Rename the session ASAP so multiple review sessions are easy to tell apart.
			const prInfo = await fetchPrInfo(pi, url.trim());
			if (prInfo) {
				pi.setSessionName(`PR #${prInfo.number}: ${prInfo.title}`);
			} else {
				ctx.ui.notify("Could not fetch PR info via gh; session name unchanged", "warning");
			}

			const language = await pickLanguage(ctx);
			if (!language || !language.trim()) {
				ctx.ui.notify("Cancelled: no language selected", "warning");
				return;
			}

			const prompt = buildPrompt(language.trim(), url.trim());
			ctx.ui.setEditorText(prompt);
			ctx.ui.notify("Review prompt loaded. Edit and submit when ready.", "info");
		},
	});
}
