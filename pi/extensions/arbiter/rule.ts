import { readFileSync } from "node:fs";

import { parse as parseCel, type ParseResult } from "@marcbachmann/cel-js";
import { parse as parseYaml } from "yaml";

/** The action a rule applies when it matches a tool call. */
export type Action = "accept" | "reject" | "verify";

/**
 * A single policy rule. Rules are evaluated in order within a scope; the first
 * match wins.
 */
export interface Rule {
  /** Optional name, for readability and audit. */
  name?: string;
  /** CEL expression evaluated against the tool call. Use "true" to always match. */
  when: string;
  /** What to do when this rule matches. */
  action: Action;
  /**
   * Message associated with the action: a steering message on accept, the
   * reason reported to the model on reject, and the prompt shown on verify.
   */
  message?: string;
  /** When rejecting, also end the current turn. */
  terminate?: boolean;
}

/** An ordered list of rules for a single scope. */
export type RuleSet = Rule[];

/** A rule paired with its pre-compiled CEL condition. */
export interface CompiledRule {
  rule: Rule;
  matches: ParseResult;
}

/** An ordered list of compiled rules for a single scope. */
export type CompiledRuleSet = CompiledRule[];

/** The outcome of loading a rule file. */
export interface LoadResult {
  rules: CompiledRuleSet;
  errors: string[];
}

const ACTIONS: ReadonlySet<string> = new Set<Action>(["accept", "reject", "verify"]);

/** Validate and compile a single raw rule entry. Throws on any problem. */
function compileRule(raw: unknown, index: number): CompiledRule {
  if (typeof raw !== "object" || raw === null) {
    throw new Error(`rule ${index}: expected a mapping`);
  }
  const r = raw as Record<string, unknown>;

  if (typeof r.when !== "string") {
    throw new Error(`rule ${index}: "when" must be a string`);
  }
  if (typeof r.action !== "string" || !ACTIONS.has(r.action)) {
    throw new Error(`rule ${index}: "action" must be one of accept, reject, verify`);
  }
  if (r.name !== undefined && typeof r.name !== "string") {
    throw new Error(`rule ${index}: "name" must be a string`);
  }
  if (r.message !== undefined && typeof r.message !== "string") {
    throw new Error(`rule ${index}: "message" must be a string`);
  }
  if (r.terminate !== undefined && typeof r.terminate !== "boolean") {
    throw new Error(`rule ${index}: "terminate" must be a boolean`);
  }

  let matches: ParseResult;
  try {
    matches = parseCel(r.when);
  } catch (err) {
    throw new Error(`rule ${index}: invalid CEL in "when": ${(err as Error).message}`);
  }

  const rule: Rule = {
    name: r.name as string | undefined,
    when: r.when,
    action: r.action as Action,
    message: r.message as string | undefined,
    terminate: r.terminate as boolean | undefined,
  };

  return { rule, matches };
}

/**
 * Load and compile a rule file.
 *
 * A missing file yields an empty set with no error. A file that cannot be read
 * or parsed as YAML yields an empty set with one error. Individual malformed
 * rules are skipped, each contributing an error; valid rules are kept.
 */
export function loadRuleSet(path: string): LoadResult {
  let content: string;
  try {
    content = readFileSync(path, "utf8");
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code === "ENOENT") {
      return { rules: [], errors: [] };
    }
    return { rules: [], errors: [`${path}: ${(err as Error).message}`] };
  }

  let doc: unknown;
  try {
    doc = parseYaml(content);
  } catch (err) {
    return { rules: [], errors: [`${path}: invalid YAML: ${(err as Error).message}`] };
  }

  if (typeof doc !== "object" || doc === null || !Array.isArray((doc as Record<string, unknown>).rules)) {
    return { rules: [], errors: [`${path}: expected a top-level "rules" list`] };
  }

  const rawRules = (doc as Record<string, unknown>).rules as unknown[];
  const rules: CompiledRuleSet = [];
  const errors: string[] = [];

  rawRules.forEach((raw, index) => {
    try {
      rules.push(compileRule(raw, index));
    } catch (err) {
      errors.push(`${path}: ${(err as Error).message}`);
    }
  });

  return { rules, errors };
}
