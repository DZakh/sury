// The issues family: a Result that reports every failure, held against the
// throwing operation and against itself.
//
//   first         `parseAsResult` accepts exactly what `parseOrThrow` does, and
//                 its first issue is the error `parseOrThrow` throws - the one
//                 failure both find before the two diverge.
//   standard      `~standard.validate` answers what `parseAsResult` does: a
//                 Result is a Standard Schema result, so they are one operation.
//   independent   for an object or array value, breaking fields (items) one at
//                 a time and then all together reports each one's own issue in
//                 the combined answer. A container collects its children; it does not let
//                 one child's failure decide what another reports.
//   sound         the other direction: breaking one field of an accepted value
//                 reports nothing under another field, and every issue of the
//                 all-broken answer is one some field reports alone. A root
//                 union is exempt from the first half: breaking its tag picks
//                 another case, which answers about its own fields.
//
// The values are the Input samples and each of them broken: a plain object or
// array with every own field or item in turn replaced by a symbol no leaf
// schema accepts. The grammar draws no async member, so the async join is held
// by specs alone.

import { show } from "../unionFuzz/sample";
import { type Ctx, type Family, reason } from "./context";

type Issue = { message: string; path?: PropertyKey[] };
type Result = { success: boolean; value?: unknown; error?: Error; issues?: Issue[] };
type Op = (value: unknown) => unknown;

const junk = Symbol("junk");
const isPlain = (value: unknown): value is Record<string, unknown> =>
  !!value && typeof value === "object" && Object.getPrototypeOf(value) === Object.prototype;
const said = (issue: Issue): string => `${(issue.path ?? []).map(String).join(".")}: ${issue.message}`;

const check = (ctx: Ctx): void => {
  const { S, schema, inputs, report, compile, count } = ctx;
  const orThrow = compile<Op>("first", () => S.parseOrThrow(schema));
  const asResult = compile<Op>("first", () => S.parseAsResult(schema));
  if (!orThrow || !asResult) return;
  const validate = (schema as { "~standard": { validate: Op } })["~standard"].validate;

  const run = (value: unknown): Result | undefined => {
    let result: Result;
    try {
      result = asResult(value) as Result;
    } catch (error) {
      report("first", `parseAsResult threw on ${show(value)} - ${reason(error)}`);
      return;
    }
    let thrown: unknown, threw = false;
    try {
      orThrow(value);
    } catch (error) {
      thrown = error;
      threw = true;
    }
    count("results");
    if (result.success === threw) {
      report("first", `parseAsResult ${result.success ? "accepted" : "rejected"} ${show(value)}, parseOrThrow ${threw ? `threw ${reason(thrown)}` : "accepted it"}`);
    } else if (threw && result.error?.message !== (thrown as Error).message) {
      report("first", `first issue of ${show(value)} is ${result.error?.message}, parseOrThrow threw ${reason(thrown)}`);
    }
    const standard = validate(value) as Result;
    if (
      standard.success !== result.success ||
      (standard.issues ?? []).map(said).join("|") !== (result.issues ?? []).map(said).join("|")
    ) {
      report("standard", `~standard.validate and parseAsResult disagree on ${show(value)}`);
    }
    return result;
  };

  for (const [value] of inputs) {
    const whole = run(value);
    const isArray = Array.isArray(value);
    if (!isPlain(value) && !isArray) continue;
    const keys = Object.keys(value);
    if (keys.length < 2) continue;
    const broken = (only?: string): unknown => {
      const copy: any = isArray ? [...(value as unknown[])] : { ...(value as object) };
      for (const key of only === undefined ? keys : [only]) copy[key] = junk;
      return copy;
    };
    const alone: string[] = [];
    const reported = new Set<string>();
    for (const key of keys) {
      const one = run(broken(key));
      if (!one || one.success) continue;
      if (String(one.issues?.[0]?.path?.[0]) === key) alone.push(said(one.issues![0]!));
      for (const issue of one.issues ?? []) {
        reported.add(said(issue));
        const head = issue.path?.[0];
        if (whole?.success && ctx.shape.name !== "union" && head !== undefined && String(head) !== key) {
          report("sound", `breaking only ${key} of ${show(value)} reports ${said(issue)}`);
        }
      }
    }
    const all = run(broken());
    // Only where the container itself stayed an object the schema reads field
    // by field: a failure at the root (a union, a refine over the whole value)
    // is one answer about all of them.
    if (!all || all.success || !all.issues?.[0]?.path?.length) continue;
    const combined = new Set(all.issues.map(said));
    for (const issue of alone) {
      if (!combined.has(issue)) {
        report("independent", `${issue} is reported alone but not with every field of ${show(value)} broken (${[...combined].join("; ")})`);
      }
    }
    for (const issue of combined) {
      if (!reported.has(issue)) {
        report("sound", `${issue} is reported with every field of ${show(value)} broken but by no field alone`);
      }
    }
  }
};

export const issues: Family = { check };
