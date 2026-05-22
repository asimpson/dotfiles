/**
 * Publish to GitHub Extension
 *
 * Publish the current pi session as HTML to a GitHub gh-pages branch.
 *
 * Auth is delegated to `gh`. The extension reads a dedicated token from
 * ~/.pi/agent/publish-to-github.token and temporarily exposes it as GH_TOKEN
 * while running `gh`.
 */

import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { createHash } from "node:crypto";
import { homedir, tmpdir } from "node:os";
import { join } from "node:path";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

const TOKEN_FILE = "~/.pi/agent/publish-to-github.token";
const PAGES_DEPLOY_TIMEOUT_MS = 5 * 60 * 1000;
const PAGES_DEPLOY_POLL_MS = 5 * 1000;

type PublishToken = {
  token: string;
  source: string;
};

type PagesInfo = {
  status: string;
  htmlUrl: string;
};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/**
 * Sanitize a user-provided string into a safe filename stem.
 * Only lowercase alphanumerics, underscores, and hyphens survive.
 */
function safeFilename(input: string): string {
  return (
    input
      .replace(/[^a-zA-Z0-9_-]/g, "_")   // keep alnum, underscore, hyphen
      .replace(/_{2,}/g, "_")             // collapse repeated _
      .replace(/^_|_$/g, "")               // trim leading / trailing _
      .toLowerCase()
  ) || "session";
}

function expandHome(path: string): string {
  if (path === "~") return homedir();
  if (path.startsWith("~/")) return join(homedir(), path.slice(2));
  return path;
}

async function readTokenFile(path: string): Promise<string> {
  let token: string;
  try {
    token = (await readFile(path, "utf8")).trim();
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    throw new Error(`Could not read GitHub token file ${path}: ${message}`);
  }

  if (!token) throw new Error(`GitHub token file ${path} is empty`);
  return token;
}

async function getPublishToken(): Promise<PublishToken> {
  const path = expandHome(TOKEN_FILE);
  return { token: await readTokenFile(path), source: path };
}

async function withGhToken<T>(publishToken: PublishToken, fn: () => Promise<T>): Promise<T> {
  const oldGhToken = process.env.GH_TOKEN;
  process.env.GH_TOKEN = publishToken.token;
  try {
    return await fn();
  } finally {
    if (oldGhToken === undefined) {
      delete process.env.GH_TOKEN;
    } else {
      process.env.GH_TOKEN = oldGhToken;
    }
  }
}

async function runGh(pi: ExtensionAPI, args: string[], publishToken: PublishToken) {
  return withGhToken(publishToken, () => pi.exec("gh", args));
}

async function detectGithubUser(
  pi: ExtensionAPI,
  publishToken: PublishToken,
): Promise<string | null> {
  try {
    const result = await runGh(pi, ["api", "user", "-q", ".login"], publishToken);
    const user = result.stdout.trim();
    return result.code === 0 && user ? user : null;
  } catch {
    return null;
  }
}

async function getExistingFileSha(
  pi: ExtensionAPI,
  repo: string,
  branch: string,
  path: string,
  publishToken: PublishToken,
): Promise<string | null> {
  const result = await runGh(pi, [
    "api",
    "--method",
    "GET",
    `repos/${repo}/contents/${path}`,
    "-f",
    `ref=${branch}`,
    "-q",
    ".sha",
  ], publishToken);

  if (result.code === 0) {
    return result.stdout.trim() || null;
  }

  const output = `${result.stdout}\n${result.stderr}`;
  if (output.includes("HTTP 404") || output.includes("Not Found")) {
    return null;
  }

  throw new Error(`Could not check existing file:\n${result.stderr || result.stdout}`);
}

function gitBlobSha(content: Buffer): string {
  return createHash("sha1")
    .update(`blob ${content.length}\0`)
    .update(content)
    .digest("hex");
}

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

function parsePagesInfo(output: string): PagesInfo {
  let data: { status?: unknown; html_url?: unknown };
  try {
    data = JSON.parse(output) as { status?: unknown; html_url?: unknown };
  } catch (error) {
    throw new Error(`Could not parse GitHub Pages status: ${formatError(error)}`);
  }

  const status = typeof data.status === "string" && data.status.trim()
    ? data.status.trim()
    : "unknown";
  const htmlUrl = typeof data.html_url === "string" && data.html_url.trim()
    ? data.html_url.trim()
    : null;

  if (!htmlUrl) {
    throw new Error("GitHub Pages API response did not include html_url");
  }

  return { status, htmlUrl };
}

async function getPagesInfo(
  pi: ExtensionAPI,
  repo: string,
  publishToken: PublishToken,
): Promise<PagesInfo> {
  const result = await runGh(pi, [
    "api",
    "--method",
    "GET",
    `repos/${repo}/pages`,
  ], publishToken);

  if (result.code !== 0) {
    throw new Error(`Could not get GitHub Pages status:\n${result.stderr || result.stdout}`);
  }

  return parsePagesInfo(result.stdout);
}

function pagesFileUrl(pagesBaseUrl: string, path: string): string {
  return `${pagesBaseUrl.replace(/\/+$/, "")}/${path}`;
}

function isPagesReady(status: string): boolean {
  return ["built", "deployed"].includes(status.toLowerCase());
}

function isPagesFailure(status: string): boolean {
  return ["error", "errored", "failed", "failure"].includes(status.toLowerCase());
}

async function waitForPagesDeployment(
  pi: ExtensionAPI,
  repo: string,
  publishToken: PublishToken,
): Promise<{ ok: true; info: PagesInfo } | { ok: false; info: PagesInfo | null }> {
  const deadline = Date.now() + PAGES_DEPLOY_TIMEOUT_MS;
  let lastInfo: PagesInfo | null = null;

  while (Date.now() < deadline) {
    lastInfo = await getPagesInfo(pi, repo, publishToken);
    if (isPagesReady(lastInfo.status)) return { ok: true, info: lastInfo };
    if (isPagesFailure(lastInfo.status)) return { ok: false, info: lastInfo };

    const remaining = deadline - Date.now();
    if (remaining <= 0) break;
    await sleep(Math.min(PAGES_DEPLOY_POLL_MS, remaining));
  }

  return { ok: false, info: lastInfo };
}

async function commitFileWithGh(
  pi: ExtensionAPI,
  repo: string,
  branch: string,
  path: string,
  message: string,
  content: Buffer,
  existingSha: string | null,
  payloadPath: string,
  publishToken: PublishToken,
) {
  await writeFile(payloadPath, JSON.stringify({
    message,
    content: content.toString("base64"),
    branch,
    ...(existingSha ? { sha: existingSha } : {}),
  }));

  return runGh(pi, [
    "api",
    "--method",
    "PUT",
    `repos/${repo}/contents/${path}`,
    "--input",
    payloadPath,
    "--silent",
  ], publishToken);
}

/**
 * Resolve the repo shorthand (owner/repo) when the user provides a full
 * HTTPS URL instead. If it already looks like "owner/repo", pass it through.
 */
function normalizeRepo(repo: string): string {
  // https://github.com/owner/repo  or  git@github.com:owner/repo.git
  const httpsMatch = repo.match(
    /github\.com[/:]([^/]+)\/([^/.]+)(?:\.git)?$/,
  );
  if (httpsMatch) return `${httpsMatch[1]}/${httpsMatch[2]}`;
  return repo;
}

function formatError(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}

// ---------------------------------------------------------------------------
// Command
// ---------------------------------------------------------------------------

export default function (pi: ExtensionAPI) {
  pi.registerCommand("publish-to-github", {
    description:
      "Export the current session as HTML and publish it to a GitHub gh-pages branch",
    handler: async (_args, ctx) => {
      if (!ctx.hasUI) {
        ctx.ui.notify(
          "publish-to-github requires interactive TUI mode",
          "error",
        );
        return;
      }

      const sessionFile = ctx.sessionManager.getSessionFile();
      if (!sessionFile) {
        ctx.ui.notify(
          "This session is in-memory and cannot be exported.",
          "error",
        );
        return;
      }

      // Ensure any pending writes are flushed before exporting.
      await ctx.waitForIdle();

      let workRoot: string | null = null;

      try {
        ctx.ui.setStatus("publish-to-github", "Configuring …");
        const rawRepo = await ctx.ui.input("GitHub repository", "owner/repo");
        if (!rawRepo) {
          ctx.ui.notify("Cancelled", "info");
          return;
        }

        const repo = normalizeRepo(rawRepo.trim());
        if (!repo) {
          ctx.ui.notify("Cancelled", "info");
          return;
        }

        const branch = await ctx.ui.input("Branch name", "gh-pages");
        const branchName = branch?.trim();
        if (!branchName) {
          ctx.ui.notify("Cancelled", "info");
          return;
        }

        ctx.ui.setStatus("publish-to-github", "Resolving auth …");
        const publishToken = await getPublishToken();
        ctx.ui.notify(`Using GitHub publish token from ${publishToken.source}`, "info");

        let username = await detectGithubUser(pi, publishToken);
        if (!username) {
          const prompt = await ctx.ui.input("GitHub username", "");
          username = prompt?.trim() ?? "";
          if (!username) {
            ctx.ui.notify("Cancelled", "info");
            return;
          }
        }
        ctx.ui.notify(`Using GitHub username: ${username}`, "info");

        const sessionName = await ctx.ui.input(
          "Session name",
          pi.getSessionName() || "session",
        );
        if (!sessionName) {
          ctx.ui.notify("Cancelled", "info");
          return;
        }

        workRoot = await mkdtemp(join(tmpdir(), "pi-publish-"));
        const htmlFilename = `${safeFilename(sessionName)}.html`;
        const outputPath = join(workRoot, htmlFilename);
        const payloadPath = join(workRoot, "github-contents-payload.json");

        ctx.ui.setStatus("publish-to-github", "Exporting session …");
        // `pi --export <input.jsonl> [output.html]` is a non-interactive CLI
        // operation. Invoking it as a subprocess is safe even while pi is
        // already running.
        const exportResult = await pi.exec("pi", [
          "--export",
          sessionFile,
          outputPath,
        ]);

        if (exportResult.code !== 0) {
          ctx.ui.notify(`Export failed:\n${exportResult.stderr}`, "error");
          return;
        }

        const relativeHtmlPath = `@${username}/${htmlFilename}`;
        const html = await readFile(outputPath);

        ctx.ui.setStatus("publish-to-github", "Checking existing file …");
        const existingSha = await getExistingFileSha(
          pi,
          repo,
          branchName,
          relativeHtmlPath,
          publishToken,
        );

        if (existingSha && existingSha === gitBlobSha(html)) {
          ctx.ui.notify("No new changes to commit (file already up to date)", "warning");
          const pagesInfo = await getPagesInfo(pi, repo, publishToken);
          ctx.ui.notify(
            `Already published (${pagesInfo.status}): ${pagesFileUrl(pagesInfo.htmlUrl, relativeHtmlPath)}`,
            "info",
          );
          return;
        }

        ctx.ui.setStatus("publish-to-github", "Committing via GitHub API …");
        const commitResult = await commitFileWithGh(
          pi,
          repo,
          branchName,
          relativeHtmlPath,
          `Publish session "${sessionName}" from pi`,
          html,
          existingSha,
          payloadPath,
          publishToken,
        );

        if (commitResult.code !== 0) {
          ctx.ui.notify(`Publish failed:\n${commitResult.stderr}`, "error");
          return;
        }

        ctx.ui.setStatus("publish-to-github", "Waiting for GitHub Pages …");
        const deployResult = await waitForPagesDeployment(pi, repo, publishToken);
        const pagesInfo = deployResult.info;
        if (!deployResult.ok || !pagesInfo) {
          const status = pagesInfo ? pagesInfo.status : "unknown";
          const url = pagesInfo
            ? pagesFileUrl(pagesInfo.htmlUrl, relativeHtmlPath)
            : "GitHub Pages URL unavailable";
          ctx.ui.notify(
            `Published commit, but GitHub Pages status is ${status}:\n${url}`,
            "warning",
          );
          return;
        }

        ctx.ui.notify(
          `Published! (${pagesInfo.status}) ${pagesFileUrl(pagesInfo.htmlUrl, relativeHtmlPath)}`,
          "info",
        );
      } catch (error) {
        ctx.ui.notify(formatError(error), "error");
      } finally {
        ctx.ui.setStatus("publish-to-github", "");
        if (workRoot) {
          await rm(workRoot, { recursive: true, force: true });
        }
      }
    },
  });

}
