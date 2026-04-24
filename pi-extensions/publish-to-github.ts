/**
 * Publish to GitHub Extension
 *
 * Publish the current pi session as HTML to a GitHub gh-pages branch.
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

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

/**
 * Derive the GitHub username from the `gh` CLI.
 * Returns undefined when `gh` is unavailable or not authenticated.
 */
async function tryDetectGithubUser(
  pi: ExtensionAPI,
): Promise<string | null> {
  try {
    const result = await pi.exec("gh", [
      "api",
      "user",
      "-q",
      ".login",
    ]);
    const user = result.stdout.trim();
    return result.code === 0 && user ? user : null;
  } catch {
    return null;
  }
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

/**
 * Execute a git command *inside* a working directory.
 * Returns the raw exit code so the caller can distinguish "no changes" from "error".
 */
async function runGit(
  pi: ExtensionAPI,
  cwd: string,
  args: string[],
) {
  return pi.exec("git", ["-C", cwd, ...args]);
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

      // ------------------------------------------------------------------
      // Pre-flight
      // ------------------------------------------------------------------
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

      // ------------------------------------------------------------------
      // 1. GitHub repository
      // ------------------------------------------------------------------
      ctx.ui.setStatus("publish-to-github", "Configuring …");
      const rawRepo = await ctx.ui.input(
        "GitHub repository",
        "owner/repo",
      );
      if (!rawRepo) {
        ctx.ui.notify("Cancelled", "info");
        ctx.ui.setStatus("publish-to-github", "");
        return;
      }
      const repo = normalizeRepo(rawRepo.trim());

      // ------------------------------------------------------------------
      // 2. Target branch
      // ------------------------------------------------------------------
      const branch = await ctx.ui.input(
        "Branch name",
        "gh-pages",
      );
      if (!branch) {
        ctx.ui.notify("Cancelled", "info");
        ctx.ui.setStatus("publish-to-github", "");
        return;
      }

      // ------------------------------------------------------------------
      // 3. Shallow clone
      // ------------------------------------------------------------------
      const tmpDir = `/tmp/pi-publish-${Date.now()}`;
      const cloneUrl = `https://github.com/${repo}.git`;

      ctx.ui.setStatus("publish-to-github", "Cloning repository …");
      const cloneResult = await pi.exec("git", [
        "clone",
        "--branch",
        branch,
        "--single-branch",
        "--depth",
        "1",
        cloneUrl,
        tmpDir,
      ]);

      if (cloneResult.code !== 0) {
        ctx.ui.notify(
          `Clone failed:\n${cloneResult.stderr}`,
          "error",
        );
        ctx.ui.setStatus("publish-to-github", "");
        return;
      }

      // ------------------------------------------------------------------
      // 4. Session name → HTML filename
      // ------------------------------------------------------------------
      ctx.ui.setStatus("publish-to-github", "Exporting session …");
      const sessionName = await ctx.ui.input(
        "Session name",
        pi.getSessionName() || "session",
      );
      if (!sessionName) {
        ctx.ui.notify("Cancelled", "info");
        await pi.exec("rm", ["-rf", tmpDir]);
        ctx.ui.setStatus("publish-to-github", "");
        return;
      }
      const htmlFilename = `${safeFilename(sessionName)}.html`;

      // ------------------------------------------------------------------
      // 5. Export session → HTML
      // ------------------------------------------------------------------
      const outputPath = `/tmp/pi-export-${Date.now()}-${htmlFilename}`;

      // `pi --export <input.jsonl> [output.html]` is a non-interactive CLI
      // operation. Invoking it as a subprocess is safe even while pi is
      // already running.
      const exportResult = await pi.exec("pi", [
        "--export",
        sessionFile,
        outputPath,
      ]);

      if (exportResult.code !== 0) {
        ctx.ui.notify(
          `Export failed:\n${exportResult.stderr}`,
          "error",
        );
        await pi.exec("rm", ["-rf", tmpDir]);
        ctx.ui.setStatus("publish-to-github", "");
        return;
      }

      ctx.ui.notify("Session exported to HTML", "success");

      // ------------------------------------------------------------------
      // 6. Derive / prompt for GitHub username
      // ------------------------------------------------------------------
      ctx.ui.setStatus("publish-to-github", "Resolving username …");
      const detectedUser = await tryDetectGithubUser(pi);
      let username = detectedUser ?? null;

      if (!username) {
        const prompt = await ctx.ui.input(
          "GitHub username",
          "",
        );
        if (!prompt) {
          ctx.ui.notify("Cancelled", "info");
          await pi.exec("rm", ["-f", outputPath]);
          await pi.exec("rm", ["-rf", tmpDir]);
          ctx.ui.setStatus("publish-to-github", "");
          return;
        }
        username = prompt.trim();
      }

      ctx.ui.notify(`Using GitHub username: ${username}`, "info");

      // ------------------------------------------------------------------
      // 7. Copy HTML into @username/ and commit
      // ------------------------------------------------------------------
      ctx.ui.setStatus("publish-to-github", "Preparing commit …");
      const userDir = `${tmpDir}/@${username}`;

      // Ensure the target directory exists
      await pi.exec("mkdir", ["-p", userDir]);
      await pi.exec("cp", [
        outputPath,
        `${userDir}/${htmlFilename}`,
      ]);

      // Ensure the working tree is configured so we can commit.
      await runGit(pi, tmpDir, [
        "config",
        "user.email",
        `${username}@users.noreply.github.com`,
      ]);
      await runGit(pi, tmpDir, [
        "config",
        "user.name",
        username,
      ]);

      // Add & commit.
      const addResult = await runGit(pi, tmpDir, [
        "add",
        `${userDir}/${htmlFilename}`,
      ]);

      if (addResult.code !== 0) {
        ctx.ui.notify(`git add failed:\n${addResult.stderr}`, "error");
        ctx.ui.setStatus("publish-to-github", "");
        return;
      }

      const commitResult = await runGit(pi, tmpDir, [
        "commit",
        "-m",
        `Publish session "${sessionName}" from pi`,
      ]);

      // "nothing to commit" is fine (file already there).
      const isNoop =
        commitResult.stderr.includes("nothing added") ||
        commitResult.stderr.includes("nothing to commit");

      if (commitResult.code !== 0 && !isNoop) {
        ctx.ui.notify(`git commit failed:\n${commitResult.stderr}`, "error");
        ctx.ui.setStatus("publish-to-github", "");
        return;
      }

      if (isNoop) {
        ctx.ui.notify("No new changes to commit (file may already exist)", "warning");
      } else {
        ctx.ui.notify("Committed", "info");
      }

      // ------------------------------------------------------------------
      // 8. Push
      // ------------------------------------------------------------------
      ctx.ui.setStatus("publish-to-github", "Pushing …");
      const pushResult = await runGit(pi, tmpDir, [
        "push",
        "origin",
        branch,
      ]);

      if (pushResult.code !== 0) {
        ctx.ui.notify(`Push failed:\n${pushResult.stderr}`, "error");
        ctx.ui.setStatus("publish-to-github", "");
        return;
      }

      // ------------------------------------------------------------------
      // Done — notify the user
      // ------------------------------------------------------------------
      ctx.ui.setStatus("publish-to-github", "");
      ctx.ui.notify(
        `Published! https://github.com/${repo}/tree/${branch}/@${username}/${htmlFilename}`,
        "success",
      );

      // Cleanup temp artifacts
      await pi.exec("rm", ["-rf", tmpDir]);
      await pi.exec("rm", ["-f", outputPath]);
    },
  });

}
