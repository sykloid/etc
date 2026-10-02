/**
 * arbiter
 *
 * Intercepts tool calls and evaluates them against a policy. Each call is
 * accepted, rejected, or sent to the user for verification.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

import { Arbiter, type ToolCall } from "./arbiter.js";

export default function (pi: ExtensionAPI) {
  const arbiter = new Arbiter();

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

    arbiter.defaultHandler.enrich(toolCall);

    const approved = await arbiter.defaultHandler.verify(toolCall, ctx);

    pi.appendEntry("arbiter", {
      toolCall,
      action: "verify",
      result: approved ? "accepted" : "rejected",
    });

    if (!approved) {
      return {
        block: true,
        terminate: true,
        reason: "arbiter: tool call rejected by user.",
      };
    }

    return undefined;
  });
}
