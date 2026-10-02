import type { ExtensionContext } from "@earendil-works/pi-coding-agent";

/** A tool call under evaluation, enriched with derived attributes. */
export interface ToolCall {
  /** The tool being invoked, e.g. "bash", "read", "write". */
  toolName: string;
  /** Unique id for this tool call. */
  toolCallId: string;
  /** Set when another tool (such as a codemode script) issued this call. */
  parentToolCallId?: string;
  /** Working directory of the session at the time of the call. */
  cwd: string;
  /** Raw tool arguments. */
  input: Record<string, unknown>;
  /** Attributes derived by the tool's enricher. Empty until enrichment runs. */
  attributes: Record<string, unknown>;
}

/**
 * Tool-specific behavior: enrichment of a tool call's attributes, and the
 * interactive UI shown when a call must be verified. The base class provides
 * defaults; subclasses override per tool.
 */
export class ToolHandler {
  /** Populate `toolCall.attributes` with derived data. Default: no-op. */
  enrich(_toolCall: ToolCall): void {}

  /**
   * Ask the user whether to allow this tool call. Default: a generic
   * confirmation dialog.
   */
  async verify(toolCall: ToolCall, ctx: ExtensionContext): Promise<boolean> {
    return ctx.ui.confirm("arbiter", `Allow ${toolCall.toolName}?`);
  }
}
