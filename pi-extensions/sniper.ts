import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { existsSync, realpathSync, statSync } from "node:fs";
import { homedir } from "node:os";
import { basename, dirname, isAbsolute, relative, resolve } from "node:path";

type Rule = {
	raw: string;
	path: string;
	kind: "file" | "dir";
};

const STRICT_ALLOWED_TOOLS = new Set(["read", "grep", "find", "ls", "edit", "write", "gcx"]);
const STATE_TYPE = "sniper-allowlist";
const SNIPER_ADD_COMMAND = "sniper-add";
const SNIPER_REMOVE_COMMAND = "sniper-remove";

function stripAt(path: string): string {
	return path.startsWith("@") ? path.slice(1) : path;
}

function expandHome(path: string): string {
	if (path === "~") return homedir();
	if (path.startsWith("~/")) return `${homedir()}${path.slice(1)}`;
	return path;
}

function resolvePath(cwd: string, path: string): string {
	const clean = expandHome(stripAt(path));
	return isAbsolute(clean) ? clean : resolve(cwd, clean);
}

function canonicalize(path: string): string {
	const abs = resolve(path);

	if (existsSync(abs)) {
		try {
			return realpathSync.native(abs);
		} catch {
			return abs;
		}
	}

	let current = abs;
	const missing: string[] = [];

	while (!existsSync(current)) {
		const parent = dirname(current);
		if (parent === current) return abs;

		missing.unshift(basename(current));
		current = parent;
	}

	try {
		return resolve(realpathSync.native(current), ...missing);
	} catch {
		return abs;
	}
}

function isInside(parent: string, child: string): boolean {
	const rel = relative(parent, child);
	return rel === "" || (!!rel && !rel.startsWith("..") && !isAbsolute(rel));
}

function parseCliSpecList(spec: string): string[] {
	return spec
		.split(",")
		.map((part) => part.trim())
		.filter(Boolean);
}

function parseCommandSpecList(spec: string): string[] {
  //handle `,` and ` ` separation
	return spec
		.split(/[,\s]+/)
		.map((part) => part.trim())
		.filter(Boolean);
}

function ruleKey(rule: Pick<Rule, "kind" | "path">): string {
	return `${rule.kind}:${rule.path}`;
}

// specs is an array of paths that are files or directories
function parseRulesFromSpecs(specs: string[], cwd: string): Rule[] {
	const rules: Rule[] = [];
	const seen = new Set<string>();

	for (const raw of specs) {
		const abs = resolvePath(cwd, raw);

		const rule: Rule = {
			raw,
			path: canonicalize(abs),
			kind: (statSync(abs).isDirectory()) ? "dir" : "file",
		};

		const key = ruleKey(rule);
		if (seen.has(key)) continue;

		seen.add(key);
		rules.push(rule);
	}

	return rules;
}

function allowedByRule(rule: Rule, target: string): boolean {
	if (rule.kind === "file") return target === rule.path;
	return isInside(rule.path, target);
}

function getInputPath(input: unknown): string | undefined {
	if (!input || typeof input !== "object") return undefined;
	const maybe = (input as { path?: unknown }).path;
	return typeof maybe === "string" ? maybe : undefined;
}

function uniqueSpecs(specs: string[]): string[] {
	const seen = new Set<string>();
	const result: string[] = [];

	for (const spec of specs) {
    if (seen.has(spec)) continue;

		seen.add(spec);
		result.push(spec);
	}

	return result;
}

function getStringArray(value: unknown): string[] {
	return Array.isArray(value) ? value.filter((item): item is string => typeof item === "string") : [];
}

function getSessionAddedSpecs(ctx: ExtensionContext): string[] {
	let addedSpecs: string[] = [];

	for (const entry of ctx.sessionManager.getBranch()) {
		if (entry.type !== "custom" || entry.customType !== STATE_TYPE) continue;

		const data = (entry as { data?: { addedSpecs?: unknown } }).data;
		if (!Array.isArray(data?.addedSpecs)) continue;

		addedSpecs = getStringArray(data.addedSpecs);
	}

	return uniqueSpecs(addedSpecs);
}

function filterSpecsByRuleKeys(specs: string[], keys: Set<string>, cwd: string): string[] {
	return specs.filter((spec) => {
		const [rule] = parseRulesFromSpecs([spec], cwd);
		return !rule || !keys.has(ruleKey(rule));
	});
}

function ensureSlash(path: string): string {
	return /[\/\\]$/.test(path) ? path : `${path}/`;
}

function formatRuleForPrompt(rule: Rule): string {
	return `${rule.kind}:${rule.raw}`;
}

function formatRuleForList(rule: Rule): string {
	return rule.kind === "dir" ? `  dir:  ${ensureSlash(rule.raw)}` : `  file: ${rule.raw}`;
}

function formatDisplay(rules: Rule[]): string {
	return rules.length > 0 ? rules.map(formatRuleForPrompt).join(", ") : "(none)";
}

function formatAllowlist(rules: Rule[]): string {
	return rules.length > 0 ? rules.map(formatRuleForList).join("\n") : "(none)";
}

function formatStatus(ruleCount: number): string {
	return `🎯 ${ruleCount} ${ruleCount === 1 ? "path" : "paths"}`;
}

export default function sniper(pi: ExtensionAPI) {
	pi.registerFlag("sniper", {
		description: "Enable sniper write allowlist with no initial writable paths",
		type: "boolean",
		default: false,
	});
	pi.registerFlag("sniper-target", {
		description: "Comma-separated initial sniper write allowlist. Requires --sniper.",
		type: "string",
	});

	let enabled = false;
	let sniperRequested = false;
	let baseSpecs: string[] = [];
	let addedSpecs: string[] = [];
	let rules: Rule[] = [];
	let display = "(none)";

	function rebuild(ctx: ExtensionContext) {
		rules = parseRulesFromSpecs([...baseSpecs, ...addedSpecs], ctx.cwd);
		display = formatDisplay(rules);
		enabled = sniperRequested;

		if (enabled) {
			// Strict mode: remove the agent bash tool and unknown/custom tools from the active set.
			// Explicitly allowed custom tools (such as gcx) remain available.
			// Otherwise a shell command or custom tool could write outside the allowlist.
      const availableTools = new Set(pi.getAllTools().map((tool) => tool.name));
      pi.setActiveTools([...STRICT_ALLOWED_TOOLS].filter((name) => availableTools.has(name)));
		}

		if (ctx.hasUI) {
			if (enabled) {
				ctx.ui.setStatus("sniper", ctx.ui.theme.fg("muted", formatStatus(rules.length)));
			} else {
				ctx.ui.setStatus("sniper", undefined);
			}
		}
	}

	function persistAddedSpecs() {
    //addedSpecs is an array of files
		pi.appendEntry(STATE_TYPE, { addedSpecs });
	}

	pi.registerCommand(SNIPER_ADD_COMMAND, {
		description: "Add files/directories to the sniper write allowlist for this session",
		handler: async (args, ctx) => {
			if (!enabled) {
				if (ctx.hasUI) {
					ctx.ui.notify("Sniper mode is inactive. Start pi with --sniper to use /sniper-add.", "warning");
				}
				return;
			}

      //args are files entered into the handler
			let input = args?.trim() ?? "";

			if (!input && ctx.hasUI) {
				const response = await ctx.ui.input("Add sniper path(s)", "README.md,src/pkg/");
				input = response?.trim() ?? "";
			}

			const newSpecs = parseCommandSpecList(input);

			if (newSpecs.length === 0) {
				if (ctx.hasUI) {
					ctx.ui.notify("Usage: /sniper-add README.md src/pkg/", "warning");
				}
				return;
			}

			addedSpecs = uniqueSpecs([...addedSpecs, ...newSpecs]);
			persistAddedSpecs();
			rebuild(ctx);

			if (ctx.hasUI) {
				ctx.ui.notify(`🎯 Added: ${newSpecs.join(", ")}\nCurrent allowlist: ${display}`, "info");
			}
		},
	});

	pi.registerCommand(SNIPER_REMOVE_COMMAND, {
		description: "Undo /sniper-add entries for this session",
		handler: async (args, ctx) => {
			if (!enabled) {
				if (ctx.hasUI) {
					ctx.ui.notify("Sniper mode is inactive. Start pi with --sniper to use /sniper-remove.", "warning");
				}
				return;
			}

			let input = args?.trim() ?? "";

			if (!input && ctx.hasUI) {
				const response = await ctx.ui.input("Remove sniper path(s)", "README.md,src/pkg/");
				input = response?.trim() ?? "";
			}

			const specsToRemove = parseCommandSpecList(input);

			if (specsToRemove.length === 0) {
				if (ctx.hasUI) {
					ctx.ui.notify("Usage: /sniper-remove README.md src/pkg/", "warning");
				}
				return;
			}

			const addedKeys = new Set(parseRulesFromSpecs(addedSpecs, ctx.cwd).map(ruleKey));
			const removeRules = parseRulesFromSpecs(specsToRemove, ctx.cwd);
			const matchingKeys = new Set(removeRules.map(ruleKey).filter((key) => addedKeys.has(key)));

			if (matchingKeys.size === 0) {
				if (ctx.hasUI) {
					ctx.ui.notify("No matching /sniper-add entries to remove. Use /sniper-list to see current entries.", "warning");
				}
				return;
			}

			const removedNow = removeRules.filter((rule) => matchingKeys.has(ruleKey(rule))).map((rule) => rule.raw);
			const skipped = removeRules.filter((rule) => !matchingKeys.has(ruleKey(rule))).map((rule) => rule.raw);

			addedSpecs = filterSpecsByRuleKeys(addedSpecs, matchingKeys, ctx.cwd);
			persistAddedSpecs();
			rebuild(ctx);

			if (ctx.hasUI) {
				const skippedLine = skipped.length > 0 ? `\nSkipped (not from /sniper-add): ${skipped.join(", ")}` : "";
				ctx.ui.notify(`🎯 Removed: ${removedNow.join(", ")}${skippedLine}\nCurrent allowlist: ${display}`, "info");
			}
		},
	});

	pi.registerCommand("sniper-list", {
		description: "Show the current sniper write allowlist",
		handler: async (_args, ctx) => {
			rebuild(ctx);

			if (ctx.hasUI) {
				ctx.ui.notify(
					[
						`🎯 Write allowlist: ${enabled ? "active" : "inactive"}`,
						`CLI targets: ${baseSpecs.length ? baseSpecs.join(", ") : "(none)"}`,
						`Session: ${addedSpecs.length ? addedSpecs.join(", ") : "(none)"}`,
						"",
						"Allowed paths:",
						formatAllowlist(rules),
					].join("\n"),
					"info",
				);
			}
		},
	});

	pi.on("session_start", (_event, ctx) => {
		const sniperFlag = pi.getFlag("sniper") === true;
		const targetFlag = pi.getFlag("sniper-target");
		const targetSpecs = typeof targetFlag === "string" && targetFlag.trim().length > 0 ? parseCliSpecList(targetFlag) : [];

		sniperRequested = sniperFlag;
		baseSpecs = sniperFlag ? targetSpecs : [];
		addedSpecs = sniperFlag ? getSessionAddedSpecs(ctx) : [];

		rebuild(ctx);

		if (ctx.hasUI) {
			if (!sniperFlag && targetSpecs.length > 0) {
				ctx.ui.notify("Ignoring --sniper-target because --sniper was not set.", "warning");
			}
		}

		if (enabled && ctx.hasUI) {
			if (rules.length > 0) {
				ctx.ui.notify(`🎯 Write allowlist active: ${display}\n`, "info");
			} else {
				ctx.ui.notify("🎯 Sniper mode active: no paths are writable yet. Use /sniper-add <path> to allow writes.\n", "info");
			}
		}
	});

	pi.on("before_agent_start", (event) => {
		if (!enabled) return undefined;

		const writeScope =
			rules.length > 0
				? `You may only modify these paths: ${display}. `
				: "No paths are currently writable until /sniper-add is used. ";

		return {
			systemPrompt:
				event.systemPrompt +
				`\n\nSNIPER MODE is active. ${writeScope}` +
				"Use edit/write only for allowed paths. The agent bash tool and unknown/custom tools are blocked except explicitly allowed tools such as gcx. " +
				"If you need to modify another path, stop and ask the user to run /sniper-add <path>.",
		};
	});

	pi.on("tool_call", (event, ctx) => {
		if (!enabled) return undefined;

		// Block the agent bash tool and custom tools unless explicitly allowed above.
		if (!STRICT_ALLOWED_TOOLS.has(event.toolName)) {
			return {
				block: true,
				reason: `Sniper mode blocks tool "${event.toolName}" to preserve the write allowlist.`,
			};
		}

		if (event.toolName !== "write" && event.toolName !== "edit") {
			return undefined;
		}

		const inputPath = getInputPath(event.input);
		if (!inputPath) {
			return {
				block: true,
				reason: `Sniper mode blocked ${event.toolName}: missing path.`,
			};
		}

		const target = canonicalize(resolvePath(ctx.cwd, inputPath));
		const allowed = rules.some((rule) => allowedByRule(rule, target));

		if (!allowed) {
			const reason = `Sniper mode blocked ${event.toolName} to "${inputPath}". Allowed paths: ${display}`;

			if (ctx.hasUI) {
				ctx.ui.notify(reason, "warning");
			}

			return { block: true, reason };
		}

		return undefined;
	});
}
