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

function formatOutput(stdout: string, stderr: string): string {
  const parts = [stdout.trim(), stderr.trim()].filter(Boolean);
  return parts.length > 0 ? parts.join("\n") : "sendPatch finished successfully.";
}

async function runSendPatch(pi: ExtensionAPI, signal?: AbortSignal) {
  const result = await pi.exec("sendPatch", [], { signal });
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
    },
  };
}

export default function (pi: ExtensionAPI) {
  pi.registerCommand("send-patch", {
    description: "Email the most recent git commit (HEAD) using sendPatch",
    handler: async (_args, ctx) => {
      try {
        const result = await runSendPatch(pi, ctx.signal);
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
    async execute(_toolCallId, _params, signal) {
      const result = await runSendPatch(pi, signal);
      return {
              content: [{ type: "text", text: result.message }],
              details: result.details,
      };
    },
  });
}
