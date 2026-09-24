// What the exhaustive fuzzers share: printing a value, comparing two, telling
// a Sury rejection from the compiler falling over, and the catalogs of cases
// known not to hold.
//
// A catalog is what keeps a pass honest. Every case a property cannot hold is
// listed with the reason written out, keyed by what the run prints; the run
// fails on a case not listed, on a listed case that has started to hold, and on
// a listed case that no longer runs at all. A key may name `*` for either half
// of its id.

import * as S from "../index.mjs";

export const show = (value: unknown): string => {
  if (value instanceof File) return `File(${value.name})`;
  if (value instanceof Blob) return `Blob(${value.size})`;
  if (value instanceof Date) return `Date(${value.toJSON() ?? "invalid"})`;
  if (value instanceof Uint8Array) return `Uint8Array[${value.join(",")}]`;
  if (typeof value === "bigint") return `${value}n`;
  if (Array.isArray(value)) return `[${value.map(show).join(",")}]`;
  if (value && typeof value === "object") {
    return `{${Object.entries(value)
      .map(([key, item]) => `${key}:${show(item)}`)
      .join(",")}}`;
  }
  return typeof value === "string" ? `'${value}'` : String(value);
};

// Blob identity is not object identity: `append` renames a bare Blob to "blob"
// and reads it back as a File, so bytes - and a File's name - are what must
// survive.
export const same = (a: unknown, b: unknown): boolean => {
  if (a instanceof Blob && b instanceof Blob) {
    return a.size === b.size && (a instanceof File && b instanceof File ? a.name === b.name : true);
  }
  if (a instanceof Date && b instanceof Date) return Object.is(+a, +b);
  if (a instanceof Uint8Array && b instanceof Uint8Array) {
    return a.length === b.length && a.every((byte, index) => byte === b[index]);
  }
  if (Array.isArray(a) && Array.isArray(b)) {
    return a.length === b.length && a.every((item, index) => same(item, b[index]));
  }
  if (typeof a === "object" && a !== null && typeof b === "object" && b !== null) {
    const keys = Object.keys(a);
    return (
      keys.length === Object.keys(b).length &&
      keys.every((key) => same((a as never)[key], (b as never)[key]))
    );
  }
  return Object.is(a, b) || (typeof a === "number" && isNaN(a) && isNaN(b as number));
};

// Blobs, Dates and bytes are handed over as they are: an encode that wrote
// into one would be a different finding.
export const copy = (value: unknown): unknown => {
  if (Array.isArray(value)) return value.map(copy);
  if (
    value !== null &&
    typeof value === "object" &&
    !(value instanceof Blob) &&
    !(value instanceof Date) &&
    !(value instanceof Uint8Array)
  ) {
    return Object.fromEntries(Object.entries(value).map(([key, item]) => [key, copy(item)]));
  }
  return value;
};

export type Compiled<T = unknown> = { fn?: T; rejected?: string; crash?: string };

// A Sury rejection is an answer; anything else is the compiler falling over.
export const compile = <T = unknown>(build: () => T): Compiled<T> => {
  try {
    return { fn: build() };
  } catch (error) {
    return fault(error);
  }
};

// A schema built wrong panics with a plain `Error` whose message says `[Sury]`,
// not an `S.Error`: that is still the library answering, at link time.
export const fault = (error: unknown): { rejected?: string; crash?: string } => {
  const err = error as Error;
  return err instanceof S.Error || err.message?.startsWith("[Sury]")
    ? { rejected: err.message.split("\n")[0] }
    : { crash: `${err.constructor.name}: ${err.message.split("\n")[0]}` };
};

// One list per property, each labelled for `--show-known`.
export type Lists = Record<string, { label: string; cases: Record<string, string> }>;

export const catalog = (lists: Lists) => {
  const used = new Set<string>();
  const findings: string[] = [];

  // The key that covers a case: its own, or one naming `*` for either half.
  // Returned rather than the reason, so two entries that share a wording are
  // still tracked apart.
  const keyFor = (list: string, a: string, b: string, value?: string): string | undefined => {
    const cases = lists[list]!.cases;
    const tail = value === undefined ? "" : ` <- ${value}`;
    return [`${a}/${b}${tail}`, `*/${b}${tail}`, `${a}/*${tail}`].find((key) => cases[key] !== undefined);
  };

  // Records a case that does not hold: excused when listed, a finding if not.
  const miss = (list: string, a: string, b: string, finding: string, value?: string): void => {
    const key = keyFor(list, a, b, value);
    if (key === undefined) findings.push(finding);
    else used.add(key);
  };

  // Records a case that holds: a finding if it was listed as not holding.
  const hold = (list: string, a: string, b: string, id: string, value?: string): void => {
    const key = keyFor(list, a, b, value);
    if (key !== undefined) {
      used.add(key);
      findings.push(`${id}: listed in ${list} but holds - delete the entry`);
    }
  };

  const finish = (summary: string): void => {
    for (const [name, { cases }] of Object.entries(lists)) {
      for (const key of Object.keys(cases)) {
        if (!used.has(key)) {
          findings.push(`${key}: listed in ${name} but no such case ran - the catalog moved under it`);
        }
      }
    }
    if (process.argv.includes("--show-known")) {
      for (const { label, cases } of Object.values(lists)) {
        console.log(`\n${label}:`);
        for (const [key, reason] of Object.entries(cases)) {
          console.log(`  ${key}\n    ${reason}`);
        }
      }
      console.log("");
    }
    console.log(summary);
    if (findings.length) {
      console.log(`\n${findings.length} finding(s):`);
      for (const finding of findings) console.log(`  ${finding}`);
      process.exitCode = 1;
    } else {
      console.log("No findings.");
    }
  };

  return { keyFor, miss, hold, finish, findings };
};
