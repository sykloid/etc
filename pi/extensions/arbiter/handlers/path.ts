import { isAbsolute, resolve, relative } from "node:path";

import type { ExtensionContext } from "@earendil-works/pi-coding-agent";
import { DynamicBorder } from "@earendil-works/pi-coding-agent";
import { Container, Text, matchesKey } from "@earendil-works/pi-tui";

import { ToolHandler, type ToolCall } from "../handler.js";

const CHOICES = ["Accept", "Reject"] as const;

/**
 * Handler for the path-based file tools (`read`, `write`, `edit`).
 *
 * Enriches the call with the resolved absolute path and the path relative to
 * the session's working directory, and presents a path-aware verification UI.
 */
export class PathHandler extends ToolHandler {
  enrich(toolCall: ToolCall): void {
    const path = toolCall.input.path;
    if (typeof path !== "string") {
      return;
    }

    const absolutePath = isAbsolute(path) ? path : resolve(toolCall.cwd, path);
    toolCall.attributes.absolutePath = absolutePath;
    toolCall.attributes.relativePath = relative(toolCall.cwd, absolutePath);
  }

  async verify(toolCall: ToolCall, ctx: ExtensionContext): Promise<boolean> {
    const relativePath = (toolCall.attributes.relativePath as string | undefined) ?? "";
    const absolutePath = (toolCall.attributes.absolutePath as string | undefined) ?? "";

    return ctx.ui.custom<boolean>((tui, theme, _kb, done) => {
      let selected = 0;

      const accent = (s: string) => theme.fg("accent", s);
      const topBorder = new DynamicBorder(accent);
      const bottomBorder = new DynamicBorder(accent);
      const header = new Container();
      header.addChild(new Text(theme.fg("text", ` Verify: ${toolCall.toolName} ${relativePath}`), 0, 0));
      header.addChild(new Text(theme.fg("dim", ` ${absolutePath}`), 0, 0));
      header.addChild(new Text("", 0, 0));
      const hint = new Text(theme.fg("dim", " \u2191\u2193 select \u00b7 enter confirm \u00b7 esc cancel"), 0, 0);

      const renderChoices = (width: number): string[] => {
        const lines: string[] = [];
        CHOICES.forEach((label, i) => {
          const marker = i === selected ? "\u203a " : "  ";
          const text = `${marker}${label}`;
          lines.push(...new Text(i === selected ? accent(text) : theme.fg("muted", text), 0, 0).render(width));
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
