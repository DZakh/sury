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

// Fails in the `.then` that checks the coder's result, after the first await.
const asyncNumber = S.parseAsPromiseOrReject(
  S.string.with(S.to, S.number, {
    decode: { async: async (value: string) => Number(value) },
    encode: String,
  }),
);

test("an async failure before the first await names the line that called it", async () => {
  const callsIt = async () => {
    try {
      await asyncNumber(1 as never);
      expect.unreachable();
    } catch (error) {
      return error as Error;
    }
  };
  const [header, top] = (await callsIt())!.stack!.split("\n");
  expect(header).toBe("SuryError: Expected string, received 1");
  // Taken while the call was still on the stack, so no pump sits above it.
  expect(top).toMatch(/^ {4}at callsIt \(/);
});

test("an async failure after the first await names the line that awaited it", async () => {
  const awaitsIt = async () => {
    try {
      await asyncNumber("x");
      expect.unreachable();
    } catch (error) {
      return error as Error;
    }
  };
  const [header, ...frames] = (await awaitsIt())!.stack!.split("\n");
  expect(header).toBe("SuryError: Expected number, received NaN");
  // The first frame that isn't the runtime's microtask pump is the function
  // that awaited, and the library's handler is cut off above it.
  const firstOwn = frames.find((frame) => !frame.includes("node:internal"));
  // The word `async` is the formatter's to print, and vitest's doesn't.
  expect(firstOwn).toMatch(/^ {4}at (async )?awaitsIt \(/);
  expect(frames.join("\n")).not.toContain("rejectionBoundary");
});

test("an async failure nothing awaits still carries a stack", async () => {
  // A bare `.catch` leaves V8 no `await` to thread the caller back through, so
  // there's no line of the caller's to name - but a stack is still there, as on
  // every failure that reaches you by a throw or a rejection.
  const error = await new Promise<Error>((resolve) => {
    asyncNumber("x").catch(resolve);
  });
  expect(error.message).toBe("Expected number, received NaN");
  expect(typeof error.stack).toBe("string");
  const frames = error.stack!.split("\n").slice(1);
  expect(frames.some((frame) => frame.includes("errorStack_test"))).toBe(false);
  expect(error.stack).not.toContain("rejectionBoundary");
});

test("an async failure is given its stack without rendering its reason", async () => {
  let reads = 0;
  const counted = {
    get field() {
      reads++;
      return 1;
    },
  };
  const parse = S.parseAsPromiseOrReject(
    S.string.with(S.to, S.number, { decode: { async: async () => counted as never }, encode: String }),
  );
  const error = await (async () => {
    try {
      await parse("x");
    } catch (error) {
      return error as Error;
    }
  })();

  expect("stack" in error!).toBe(true);
  expect(reads).toBe(0);
  expect(error!.message).toBe("Expected number, received { field: 1; }");
  expect(reads).toBe(1);
});

test("a coder's own async throw stays the cause, with the stack it was thrown with", async () => {
  const boom = new Error("not found");
  const parse = S.parseAsPromiseOrReject(
    S.string.with(S.to, S.number, {
      decode: {
        async: async () => {
          throw boom;
        },
      },
      encode: String,
    }),
  );
  const error = await new Promise<S.Error>((resolve) => {
    parse("x").catch(resolve);
  });

  expect(error.code).toBe("invalid_conversion");
  expect((error as { cause?: unknown }).cause).toBe(boom);
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
//
// Put back by its descriptor for the same reason the boundary writes `stack`
// with one: V8's is non-enumerable, and assigning it back would leave every
// later test in this worker reading it out of `Object.keys(Error)`.
const withoutCaptureStackTrace = async <T>(body: () => T | Promise<T>): Promise<T> => {
  const descriptor = Object.getOwnPropertyDescriptor(Error, "captureStackTrace")!;
  // @ts-expect-error - modelling an engine that never had it
  delete Error.captureStackTrace;
  try {
    return await body();
  } finally {
    Object.defineProperty(Error, "captureStackTrace", descriptor);
  }
};

test("an engine without captureStackTrace still gets a stack", async () => {
  const parse = S.parseOrThrow(user);
  await withoutCaptureStackTrace(() => {
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

test("modelling an engine without captureStackTrace leaves no trace on Error", async () => {
  const before = Object.getOwnPropertyDescriptor(Error, "captureStackTrace");
  await withoutCaptureStackTrace(() => undefined);
  expect(Object.getOwnPropertyDescriptor(Error, "captureStackTrace")).toEqual(before);
  expect(Object.keys(Error)).not.toContain("captureStackTrace");
});

test("an engine without captureStackTrace still gets a stack on an async failure", async () => {
  const error = await withoutCaptureStackTrace(async () => {
    try {
      await asyncNumber("x");
    } catch (error) {
      return error as Error;
    }
  });
  expect(typeof error!.stack).toBe("string");
  expect(Object.keys(error!)).toEqual(["code", "path", "input"]);
});
