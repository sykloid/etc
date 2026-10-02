/**
 * arbiter
 *
 * Intercepts tool calls and evaluates them against a policy. Each call is
 * accepted, rejected, or sent to the user for verification.
 */

import type {
  ExtensionAPI,
  ExtensionContext,
} from "@earendil-works/pi-coding-agent";

import { Arbiter } from "./arbiter.js";
import type { ToolCall } from "./handler.js";

export default function (pi: ExtensionAPI) {
  const arbiter = new Arbiter();

  const reload = (ctx: ExtensionContext): void => {
    const errors = arbiter.load(ctx);
    for (const error of errors) {
      ctx.ui.notify(`arbiter: ${error}`, "error");
    }
  };

  pi.on("session_start", async (_event, ctx) => {
    reload(ctx);
  });

  pi.registerCommand("arbiter", {
    description: "Manage the arbiter (reload)",
    getArgumentCompletions: (prefix) => {
      const subcommands = [{ value: "reload", label: "reload — reload rules from disk" }];
      const matches = subcommands.filter((s) => s.value.startsWith(prefix));
      return matches.length > 0 ? matches : null;
    },
    handler: async (args, ctx) => {
      const sub = args.trim().split(/\s+/)[0];
      switch (sub) {
        case "reload":
          reload(ctx);
          ctx.ui.notify("arbiter: rules reloaded.", "info");
          break;
        default:
          ctx.ui.notify("arbiter: usage: /arbiter reload", "warning");
      }
    },
  });

  pi.on("tool_call", async (event, ctx) => {
    if (!ctx.hasUI) {
      return {
        block: true,
        terminate: true,
        reason: "arbiter: no UI available to verify tool call.",
      };
    }

    const toolCall: ToolCall = {
      toolName: event.toolName,
      toolCallId: event.toolCallId,
      parentToolCallId: event.parentToolCallId,
      cwd: ctx.cwd,
      input: event.input as Record<string, unknown>,
      attributes: {},
    };

    const handler = arbiter.handlerFor(toolCall.toolName);
    handler.enrich(toolCall);

    const rule = arbiter.evaluate(toolCall);

    const audit = (result: string) =>
      pi.appendEntry("arbiter", { toolCall, rule, result });

    switch (rule.action) {
      case "accept": {
        audit("accepted");
        if (rule.message) {
          pi.sendUserMessage(rule.message, { deliverAs: "steer" });
        }
        return undefined;
      }

      case "reject": {
        audit("rejected");
        return {
          block: true,
          terminate: rule.terminate ?? false,
          reason: rule.message ?? "arbiter: tool call rejected by rule.",
        };
      }

      case "verify": {
        const approved = await handler.verify(toolCall, ctx);
        audit(approved ? "accepted" : "rejected");
        if (!approved) {
          return {
            block: true,
            terminate: true,
            reason: "arbiter: tool call rejected by user.",
          };
        }
        return undefined;
      }
    }
  });
}
