import type { ExtensionContext } from "@earendil-works/pi-coding-agent";
import { DynamicBorder } from "@earendil-works/pi-coding-agent";
import { Container, Text, matchesKey } from "@earendil-works/pi-tui";
import { parse as parseScript } from "unbash";

import { ToolHandler, type ToolCall } from "../handler.js";

const CHOICES = ["Accept", "Reject"] as const;

/** A simple command together with the operator joining it to the previous one. */
interface CommandStep {
  /** The operator preceding this command (`&&`, `||`, `|`, `;`), or "" for the first. */
  op: string;
  /** The reconstructed simple command text. */
  text: string;
}

/** A loose view of the unbash AST; only the fields we traverse are named. */
interface Node {
  type: string;
  commands?: Node[];
  command?: Node;
  operators?: string[];
  name?: { text: string };
  prefix?: Array<{ text: string }>;
  suffix?: Array<{ text: string }>;
  text?: string;
}

/** Reconstruct a simple command's text from its name, prefix, and suffix. */
function reconstructCommand(node: Node): string {
  const parts: string[] = [];
  if (node.name) parts.push(node.name.text);
  for (const p of node.prefix ?? []) parts.push(p.text);
  for (const s of node.suffix ?? []) parts.push(s.text);
  return parts.join(" ");
}

/**
 * Flatten an unbash AST into a left-to-right list of command steps, each tagged
 * with the operator that joins it to the previous step.
 */
function flatten(node: Node, inheritedOp: string, out: CommandStep[]): CommandStep[] {
  switch (node.type) {
    case "Script": {
      (node.commands ?? []).forEach((s, i) =>
        flatten(s.command ?? s, i === 0 ? inheritedOp : ";", out),
      );
      return out;
    }
    case "Statement":
      return node.command ? flatten(node.command, inheritedOp, out) : out;
    case "AndOr":
    case "Pipeline": {
      const commands = node.commands ?? [];
      const operators = node.operators ?? [];
      commands.forEach((c, i) =>
        flatten(c, i === 0 ? inheritedOp : (operators[i - 1] ?? ""), out),
      );
      return out;
    }
    case "Command":
      out.push({ op: inheritedOp, text: reconstructCommand(node) });
      return out;
    default:
      out.push({ op: inheritedOp, text: node.text ?? node.type });
      return out;
  }
}

/** Parse a command string into command steps, falling back to a single step. */
function parseCommand(command: string): CommandStep[] {
  try {
    const steps = flatten(parseScript(command) as unknown as Node, "", []);
    return steps.length > 0 ? steps : [{ op: "", text: command }];
  } catch {
    return [{ op: "", text: command }];
  }
}

/** Render command steps with operators right-aligned in a gutter. */
function renderSteps(
  steps: CommandStep[],
  theme: { fg: (color: string, text: string) => string },
): string[] {
  const gutter = Math.max(0, ...steps.map((s) => s.op.length));
  return steps.map((s) => {
    const op = s.op.padStart(gutter);
    return `${theme.fg("accent", op)} ${theme.fg("text", s.text)}`;
  });
}

/**
 * Handler for the `bash` tool.
 *
 * Presents the command in the verification UI as a left-to-right list of simple
 * commands, each prefixed by the operator (`&&`, `||`, `|`, `;`) that joins it
 * to the previous one.
 */
export class BashHandler extends ToolHandler {
  async verify(toolCall: ToolCall, ctx: ExtensionContext): Promise<boolean> {
    const command = (toolCall.input.command as string | undefined) ?? "";
    const steps = parseCommand(command);

    return ctx.ui.custom<boolean>((tui, theme, _kb, done) => {
      let selected = 0;

      const accent = (s: string) => theme.fg("accent", s);
      const topBorder = new DynamicBorder(accent);
      const bottomBorder = new DynamicBorder(accent);

      const header = new Container();
      header.addChild(new Text(theme.fg("text", " Verify: bash"), 0, 0));
      header.addChild(new Text("", 0, 0));
      for (const line of renderSteps(steps, theme)) {
        header.addChild(new Text(` ${line}`, 0, 0));
      }
      header.addChild(new Text("", 0, 0));

      const hint = new Text(
        theme.fg("dim", " \u2191\u2193 select \u00b7 enter confirm \u00b7 esc cancel"),
        0,
        0,
      );

      const renderChoices = (width: number): string[] => {
        const lines: string[] = [];
        CHOICES.forEach((label, i) => {
          const marker = i === selected ? "\u203a " : "  ";
          const text = `${marker}${label}`;
          lines.push(
            ...new Text(i === selected ? accent(text) : theme.fg("muted", text), 0, 0).render(width),
          );
        });
        return lines;
      };

      return {
        render(width: number): string[] {
          return [
            ...topBorder.render(width),
            ...header.render(width),
            ...renderChoices(width),
            ...new Text("", 0, 0).render(width),
            ...hint.render(width),
            ...bottomBorder.render(width),
          ];
        },

        handleInput(data: string) {
          if (matchesKey(data, "up")) {
            selected = (selected + CHOICES.length - 1) % CHOICES.length;
          } else if (matchesKey(data, "down")) {
            selected = (selected + 1) % CHOICES.length;
          } else if (matchesKey(data, "enter")) {
            done(selected === 0);
            return;
          } else if (matchesKey(data, "escape")) {
            done(false);
            return;
          }
          tui.requestRender();
        },

        invalidate() {},
      };
    });
  }
}
