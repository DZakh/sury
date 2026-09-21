import { expect, test } from "vitest";
import * as S from "sury";

// Where a failure gets a stack, and where it deliberately has none. A spec
// pins generated code and messages; a stack is neither, and the `try` a
// throwing operation wraps its body in only means something together with the
// frames that come out of it (see CONTRIBUTING.md's Spec Harness Suggestions).

const user = S.schema({ id: S.string });
const invalid = { id: 1 };

// `S.Error` is a union over `code`, and only the `invalid_input` arm carries
// the schemas. Narrowing here keeps each test reading as what it is about.
const invalidInput = (error: S.Error | undefined) => {
  if (error?.code !== "invalid_input") throw new Error(`Expected invalid_input, got ${error?.code}`);
  return error;
};

test("an error handed back rather than thrown carries no stack", () => {
  const result = S.parseAsResult(user, invalid);
  expect(result.success).toBe(false);
  expect(result.error).toBeInstanceOf(Error);
  expect(result.error).toBeInstanceOf(S.Error);
  expect("stack" in result.error!).toBe(false);
  expect(result.error!.message).toBe(`Failed at id: Expected string, received 1`);

  expect("stack" in (user["~standard"].validate(invalid) as object)).toBe(false);
  expect(S.isInput(user, invalid)).toBe(false);
});

test("a thrown error's stack starts at the line that called the operation", () => {
  const parse = S.parseOrThrow(user);
  const callerLine = new Error().stack!.split("\n")[1]!.replace(/:\d+:\d+\)?$/, "");
  try {
    parse(invalid);
    expect.unreachable();
  } catch (error) {
    const stack = (error as Error).stack!;
    expect(stack.split("\n")[0]).toBe(
      "SuryError: Failed at id: Expected string, received 1",
    );
    // Frame #0 is this test, not five frames of library.
    expect(stack.split("\n")[1]).toContain(callerLine.trim().replace(/^at /, ""));
  }
});

test("a stack the thrower already chose is left alone", () => {
  // `new S.Error` is the ReScript surface (`S.Error.make`); TypeScript exposes
  // the class for `instanceof` only, hence the cast.
  const SuryError = S.Error as unknown as new (details: unknown) => Error;
  const refined = S.string.with(S.refine, (value: string) => {
    if (value !== "ok")
      throw new SuryError({ code: "invalid_operation", path: [], reason: "nope" });
    return true;
  });
  try {
    S.parseOrThrow(refined, "no");
    expect.unreachable();
  } catch (error) {
    expect((error as Error).message).toBe("nope");
    // The refiner's own frame, which the throw boundary must not overwrite.
    expect((error as Error).stack).toContain("errorStack_test.ts");
    expect((error as Error).stack!.split("\n")[1]).toContain("errorStack_test.ts");
  }
});

test("a schema the compiler refuses names the line that wired it up", () => {
  // The other boundary. Raised while the operation is compiling, so the frames
  // above it are the compiler walking down to the schema with no codec - ten of
  // them, more than `Error.stackTraceLimit` allows, which used to leave the
  // caller's own line off the end of its own stack trace.
  const wireItUp = () => S.decodeOrThrow(S.boolean, S.number);
  try {
    wireItUp();
    expect.unreachable();
  } catch (error) {
    const frames = (error as Error).stack!.split("\n");
    expect((error as Error).message).toBe(
      "Can't decode boolean -> number. Define custom codec with S.to",
    );
    // The compiler's walk down to the offending schema is what the cut removes;
    // what is left above the caller is the operation it really did call through.
    expect((error as Error).stack).not.toContain("B_unsupportedDecode");
    expect(frames.findIndex((frame) => frame.includes("wireItUp"))).toBeLessThan(4);
  }
});

test("an exception that is not ours escapes with the stack it had", () => {
  const thrown = new TypeError("from a getter");
  const data = {
    get id() {
      throw thrown;
    },
  };

  expect(() => S.parseOrThrow(user, data)).toThrow(thrown);
  expect(thrown.stack!.split("\n")[1]).toContain("errorStack_test.ts");
});

test("a failure carries only what the value decided", () => {
  const { error } = S.parseAsResult(user, invalid);
  // What the check settled when it was compiled lives on its site prototype,
  // so it stays out of `console.log` and `JSON.stringify` without stopping
  // anyone reading it.
  expect(Object.keys(error!)).toEqual(["code", "path", "input"]);
  expect(invalidInput(error).expected).toBe(S.string);
  expect(Object.prototype.hasOwnProperty.call(error, "expected")).toBe(false);
  expect(error!.reason).toBe("Expected string, received 1");
  expect(error!.message).toBe("Failed at id: Expected string, received 1");
});

test("a check that names its own message says it without rendering one", () => {
  const named = S.string.with(S.meta, { errorMessage: { type: "Give me a string" } });
  expect(S.parseAsResult(named, 1).error!.reason).toBe("Give me a string");
});

test("prepending a path keeps what the failing check knew", () => {
  // The inner parse throws a compiled failure; the refiner's wrapper catches it
  // and rebuilds it with the outer path prepended. That copy used to be a
  // spread, which takes own properties only - so everything the site prototype
  // holds would have been dropped and the reason would read `Expected
  // undefined`.
  const inner = S.parseOrThrow(S.number);
  const outer = S.schema({
    a: S.unknown.with(S.refine, (value: unknown) => {
      inner(value);
      return true;
    }),
  });

  const { error } = S.parseAsResult(outer, { a: "not a number" });
  expect(error!.message).toBe(`Failed at a: Expected number, received "not a number"`);
  expect(invalidInput(error).expected).toBe(S.number);
  expect(error!.path).toEqual(["a"]);
});

test("a union failure carries what its members said", () => {
  const u = S.union([S.string, S.number]);
  const { error } = S.parseAsResult(u, { x: 1 });
  expect(error!.message).toBe("Expected string | number, received { x: 1; }");
  expect(invalidInput(error).expected).toBe(u);
});

test("an async failure past the first await rejects without one", async () => {
  const schema = S.string.with(S.to, S.number, {
    decode: { async: async (value: string) => Number(value) },
    encode: String,
  });
  const parse = S.parseAsPromiseOrReject(schema);

  // Before the first await the caller's frames are still there, so the
  // boundary takes them.
  await expect(parse(1)).rejects.toSatisfy((error: Error) => "stack" in error);
  // After it they are not, and a capture at that point yields a header line
  // and nothing else - so nothing is captured.
  await expect(parse("x")).rejects.toSatisfy((error: Error) => !("stack" in error));
});

test("an error thrown back through a parse keeps the class it was thrown as", () => {
  const SuryError = S.Error as unknown as new (details: unknown) => Error;
  class AppError extends SuryError {}
  const boom = new AppError({ code: "invalid_operation", path: [], reason: "nope" });
  const refined = S.string.with(S.refine, () => {
    throw boom;
  });

  expect(() => S.parseOrThrow(refined, "x")).toThrow("nope");
  expect(boom).toBeInstanceOf(AppError);
});

// `Error.captureStackTrace` is V8's. Deleting it is how an engine without one
// looks from here, and the boundary reads it per call so the swap takes.
const withoutCaptureStackTrace = <T>(body: () => T): T => {
  const captureStackTrace = Error.captureStackTrace;
  // @ts-expect-error - modelling an engine that never had it
  delete Error.captureStackTrace;
  try {
    return body();
  } finally {
    Error.captureStackTrace = captureStackTrace;
  }
};

test("an engine without captureStackTrace still gets a stack", () => {
  const parse = S.parseOrThrow(user);
  withoutCaptureStackTrace(() => {
    try {
      parse(invalid);
      expect.unreachable();
    } catch (error) {
      const thrown = error as S.Error;
      // No `cut` to apply, so the boundary's own frames stay on top. This test
      // is here for the stack existing at all, which is what such an engine
      // used to go without.
      expect(typeof thrown.stack).toBe("string");
      expect(thrown.stack).toContain(import.meta.url.replace("file://", ""));
      expect(thrown.message).toBe("Failed at id: Expected string, received 1");
      // `stack` is not part of what a failure reads back as, on any engine.
      expect(Object.keys(thrown)).toEqual(["code", "path", "input"]);
    }
  });
});

test("an error built by hand renders the reason it was never given", () => {
  // Nothing the compiler raises reaches the prototype-wide renderer: a
  // compiled failure carries its check's own. This is the path that does.
  const SuryError = S.Error as unknown as new (details: unknown) => S.Error;
  const error = new SuryError({
    code: "invalid_input",
    path: ["id"],
    expected: S.string,
    received: S.unknown,
    input: 1,
  });

  expect(error.reason).toBe("Expected string, received 1");
  expect(error.message).toBe("Failed at id: Expected string, received 1");
});
