import { test, expect } from "vitest";
import * as S from "../src/S.mjs";
import { tagFlags } from "../src/base";
import { unionAnyTag } from "../src/union";

// `unionAnyTag` is a hand-written literal on the hot path. If a 17th tag flag
// is ever added without widening it, every "accepts anything" mask silently
// becomes "anything except the new tag" and fallback elision starts dropping
// reachable cases — the failure is a wrong schema, not an error.
test("unionAnyTag covers every tag flag", () => {
  const all = Object.values(tagFlags).reduce((acc, flag) => acc | flag, 0);
  expect(all).toBe(unionAnyTag);
});

// Instance variants group by class identity, never by `class.name`: distinct
// classes routinely share a name (any minified bundle), and name-keyed grouping
// put the second class under the first one's `instanceof` narrow, rejecting
// every instance of it.
test("union dispatches between distinct classes sharing a name", () => {
  const A = class Foo {
    a = 1;
  };
  const B = class Foo {
    b = 2;
  };
  const parse = S.parser(S.union([S.instance(A), S.instance(B)]));

  expect(parse(new A())).toBeInstanceOf(A);
  expect(parse(new B())).toBeInstanceOf(B);
  expect(() => parse({})).toThrow("Expected Foo | Foo, received");

  // The same class listed twice still shares one narrow — identity keying must
  // not split what name keying correctly grouped.
  const again = S.parser(S.union([S.instance(A), S.instance(A)]));
  expect(again(new A())).toBeInstanceOf(A);
});
