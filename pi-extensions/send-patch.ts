/**
* send-patch.ts
*
* A very small pi extension that wraps the `sendPatch` shell command.
*
* It adds:
*   - /send-patch   -> a slash command you can run yourself
*   - send_patch    -> a tool the model can call
*
* Install location:
*   ~/.pi/agent/extensions/send-patch.ts
*
* After saving, run /reload in pi.
*/

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { Type } from "@sinclair/typebox";

type SendPatchOptions = {
  signal?: AbortSignal;
  sessionId?: string;
  cwd?: string;
};

function formatOutput(stdout: string, stderr: string): string {
  const parts = [stdout.trim(), stderr.trim()].filter(Boolean);
  return parts.length > 0 ? parts.join("\n") : "sendPatch finished successfully.";
}

async function runSendPatch(pi: ExtensionAPI, options: SendPatchOptions = {}) {
  const args: string[] = [];
  if (options.sessionId && options.cwd) {
    args.push("--session-id", options.sessionId, "--cwd", options.cwd);
  }

  const result = await pi.exec("sendPatch", args, {
    signal: options.signal,
    cwd: options.cwd,
  });
  const message = formatOutput(result.stdout, result.stderr);

  if (result.code !== 0) {
    throw new Error(message || `sendPatch failed with exit code ${result.code}`);
  }

  return {
    message,
    details: {
      code: result.code,
      stdout: result.stdout,
      stderr: result.stderr,
      sessionId: options.sessionId,
      cwd: options.cwd,
    },
  };
}

export default function (pi: ExtensionAPI) {
  pi.registerCommand("send-patch", {
    description: "Email the most recent git commit (HEAD) using sendPatch",
    handler: async (_args, ctx) => {
      try {
        const result = await runSendPatch(pi, {
          signal: ctx.signal,
          sessionId: ctx.sessionManager.getSessionId(),
          cwd: ctx.cwd,
        });
        ctx.ui.notify(result.message, "info");
      } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        ctx.ui.notify(message, "error");
      }
    },
  });

  pi.registerTool({
    name: "send_patch",
    label: "Send Patch",
    description: "Email the most recent git commit (HEAD) as a patch using sendPatch",
    promptSnippet: "Email the most recent git commit (HEAD) as a patch for review.",
    promptGuidelines: [
      "Use this only after the requested changes are committed and ready to be emailed for review.",
    ],
    parameters: Type.Object({}),
    async execute(_toolCallId, _params, signal, _onUpdate, ctx) {
      const result = await runSendPatch(pi, {
        signal,
        sessionId: ctx.sessionManager.getSessionId(),
        cwd: ctx.cwd,
      });
      return {
        content: [{ type: "text", text: result.message }],
        details: result.details,
      };
    },
  });
}
