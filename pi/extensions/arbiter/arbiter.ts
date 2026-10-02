import { homedir } from "node:os";
import { join } from "node:path";

import type { ExtensionContext } from "@earendil-works/pi-coding-agent";

import { ToolHandler } from "./handler.js";
import { PathHandler } from "./handlers/path.js";
import type { ToolCall } from "./handler.js";
import { loadRuleSet, type CompiledRuleSet, type Rule } from "./rule.js";

const GLOBAL_RULES_PATH = join(homedir(), ".pi", "agent", "arbiter", "rules.yml");
const PROJECT_RULES_RELATIVE = join(".pi", "arbiter", "rules.yml");

/** Applied when no rule matches a tool call. */
const DEFAULT_RULE: Rule = { name: "default", when: "true", action: "verify" };

export class Arbiter {
  /** Handler used for tools without a tool-specific handler. */
  private readonly defaultHandler = new ToolHandler();

  /** Tool-specific handlers, keyed by tool name. */
  private readonly handlers = new Map<string, ToolHandler>();

  /** Rule sets in evaluation order: most specific scope first. */
  private rulesets: CompiledRuleSet[] = [];

  constructor() {
    const pathHandler = new PathHandler();
    this.handlers.set("read", pathHandler);
    this.handlers.set("write", pathHandler);
    this.handlers.set("edit", pathHandler);
  }

  /** The handler for a tool, falling back to the default handler. */
  handlerFor(toolName: string): ToolHandler {
    return this.handlers.get(toolName) ?? this.defaultHandler;
  }

  /**
   * Load rule sets for the current session: the project scope (when the project
   * is trusted) followed by the global scope. Returns any errors encountered.
   */
  load(ctx: ExtensionContext): string[] {
    const rulesets: CompiledRuleSet[] = [];
    const errors: string[] = [];

    if (ctx.isProjectTrusted()) {
      const project = loadRuleSet(join(ctx.cwd, PROJECT_RULES_RELATIVE));
      rulesets.push(project.rules);
      errors.push(...project.errors);
    }

    const global = loadRuleSet(GLOBAL_RULES_PATH);
    rulesets.push(global.rules);
    errors.push(...global.errors);

    this.rulesets = rulesets;
    return errors;
  }

  /**
   * Evaluate a tool call against the loaded rule sets in order. The first rule
   * whose condition matches wins. A rule whose condition throws is treated as
   * non-matching. Returns the synthetic default rule when nothing matches.
   */
  evaluate(toolCall: ToolCall): Rule {
    for (const ruleset of this.rulesets) {
      for (const { rule, matches } of ruleset) {
        let matched = false;
        try {
          matched = matches(toolCall) === true;
        } catch {
          matched = false;
        }
        if (matched) {
          return rule;
        }
      }
    }
    return DEFAULT_RULE;
  }
}
