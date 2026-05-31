// Vendored from
// https://github.com/enviodev/hyperindex/blob/main/packages/envio/src/bindings/Vitest.res
// with an AVA-compatibility shim layered on top so existing tests keep working.

// ============================================================================
// Expectation object returned by expect()
// ============================================================================

type rec expectation<'a> = {
  toBe: 'a => unit,
  toEqual: 'a => unit,
  toStrictEqual: 'a => unit,
  toBeTruthy: unit => unit,
  toBeFalsy: unit => unit,
  toBeNull: unit => unit,
  toBeUndefined: unit => unit,
  toBeDefined: unit => unit,
  toBeGreaterThan: 'a => unit,
  toBeGreaterThanOrEqual: 'a => unit,
  toBeLessThan: 'a => unit,
  toBeLessThanOrEqual: 'a => unit,
  toBeCloseTo: (float, ~numDigits: int=?) => unit,
  toBeNaN: unit => unit,
  toContain: string => unit,
  toMatch: RegExp.t => unit,
  toMatchString: string => unit,
  toContainItem: 'a => unit,
  toHaveLength: int => unit,
  toHaveProperty: string => unit,
  toHavePropertyValue: 'b. (string, 'b) => unit,
  toThrow: unit => unit,
  toThrowError: string => unit,
  toMatchSnapshot: unit => unit,
  @as("not") not_: expectation<'a>,
}

type testContext = {expect: 'a. 'a => expectation<'a>}

// ============================================================================
// Test Suite Functions
// ============================================================================

@module("vitest")
external describe: (string, unit => unit) => unit = "describe"

@module("vitest") @scope("describe")
external describe_only: (string, unit => unit) => unit = "only"

@module("vitest") @scope("describe")
external describe_skip: (string, unit => unit) => unit = "skip"

// ============================================================================
// Test Functions (sync)
// ============================================================================

@module("vitest")
external it: (string, testContext => unit) => unit = "it"

@module("vitest") @scope("it")
external it_only: (string, testContext => unit) => unit = "only"

@module("vitest") @scope("it")
external it_skip: (string, testContext => unit) => unit = "skip"

@module("vitest")
external test: (string, testContext => unit) => unit = "test"

@module("vitest") @scope("test")
external test_only: (string, testContext => unit) => unit = "only"

@module("vitest") @scope("test")
external test_skip: (string, testContext => unit) => unit = "skip"

// ============================================================================
// Setup and Teardown
// ============================================================================

@module("vitest")
external beforeAll: (unit => unit) => unit = "beforeAll"

@module("vitest")
external afterAll: (unit => unit) => unit = "afterAll"

@module("vitest")
external beforeEach: (unit => unit) => unit = "beforeEach"

@module("vitest")
external afterEach: (unit => unit) => unit = "afterEach"

// ============================================================================
// Async Module
// ============================================================================

type options = {retry?: int}

module Async = {
  @module("vitest")
  external it: (string, testContext => promise<unit>, ~timeout: int=?) => unit = "it"

  @module("vitest")
  external itWithOptions: (string, options, testContext => promise<unit>) => unit = "it"

  @module("vitest") @scope("it")
  external it_only: (string, testContext => promise<unit>) => unit = "only"

  @module("vitest") @scope("it")
  external it_skip: (string, testContext => promise<unit>) => unit = "skip"

  @module("vitest") @scope("it")
  external it_skipIf: bool => (string, testContext => promise<unit>) => unit = "skipIf"

  @module("vitest") @scope("it")
  external it_fails: (string, testContext => promise<unit>) => unit = "fails"

  @module("vitest")
  external test: (string, testContext => promise<unit>) => unit = "test"

  @module("vitest") @scope("test")
  external test_only: (string, testContext => promise<unit>) => unit = "only"

  @module("vitest") @scope("test")
  external test_skip: (string, testContext => promise<unit>) => unit = "skip"

  @module("vitest")
  external beforeAll: (unit => promise<unit>) => unit = "beforeAll"

  @module("vitest")
  external afterAll: (unit => promise<unit>) => unit = "afterAll"

  @module("vitest")
  external beforeEach: (unit => promise<unit>) => unit = "beforeEach"

  @module("vitest")
  external afterEach: (unit => promise<unit>) => unit = "afterEach"
}

// ============================================================================
// Expect (testContext-scoped — use `t.expect(...)` from inside a test)
// ============================================================================

@send @scope("expect")
external expectAssertions: (testContext, int) => unit = "assertions"

@send @scope("expect")
external expectFail: (testContext, string) => unit = "fail"

// ============================================================================
// AVA-compatibility shim
// ============================================================================
// Kept thin so existing `open Ava` / `t->Assert.deepEqual(...)` test code keeps
// reading the same. Each shim delegates to a Vitest matcher.

let asyncTest = Async.test

module ExecutionContext = {
  type t<'a> = testContext
  let plan = (t: t<'a>, n: int) => t->expectAssertions(n)
}

type throwsExpectations<'instanceOf> = {
  message?: string,
  instanceOf?: 'instanceOf,
}

module Assert = {
  let deepEqual = (t: testContext, actual: 'a, expected: 'a, ~message as _=?) =>
    t.expect(actual).toEqual(expected)

  let unsafeDeepEqual = (t: testContext, actual: 'a, expected: 'b, ~message as _=?) =>
    t.expect(actual).toEqual(expected->Obj.magic)

  let notDeepEqual = (t: testContext, actual: 'a, expected: 'a, ~message as _=?) =>
    t.expect(actual).not_.toEqual(expected)

  let is = (t: testContext, actual: 'a, expected: 'a, ~message as _=?) =>
    t.expect(actual).toBe(expected)

  let not = (t: testContext, actual: 'a, expected: 'a, ~message as _=?) =>
    t.expect(actual).not_.toBe(expected)

  let pass = (_t: testContext, ~message as _=?) => ()

  let fail = (t: testContext, message: string) => t->expectFail(message)

  let throws = (
    t: testContext,
    fn: unit => 'a,
    ~expectations: throwsExpectations<'instanceOf>={},
    ~message as _=?,
  ) => {
    switch expectations.message {
    | Some(msg) => t.expect(fn).toThrowError(msg)
    | None => t.expect(fn).toThrow()
    }
  }

  let notThrows = (t: testContext, fn: unit => 'a, ~message as _=?) =>
    t.expect(fn).not_.toThrow()
}
