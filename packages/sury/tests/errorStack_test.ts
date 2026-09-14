import { expect, test } from "vitest";
import * as S from "sury";

// Where a failure gets a stack, and where it deliberately has none. A spec
// pins generated code and messages; a stack is neither, and the `try` a
// throwing operation wraps its body in only means something together with the
// frames that come out of it (see CONTRIBUTING.md's Spec Harness Suggestions).

const user = S.schema({ id: S.string });
const invalid = { id: 1 };

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

test("reason is rendered when it is read, not when the error is built", () => {
  const { error } = S.parseAsResult(user, invalid);
  // The ingredients are the own properties; the sentence is not one of them.
  expect(Object.keys(error!)).toEqual([
    "code",
    "expected",
    "received",
    "path",
    "unionErrors",
    "input",
  ]);
  expect(error!.reason).toBe("Expected string, received 1");
  // An explicit message is an own property, which shadows the renderer.
  const overridden = S.parseAsResult(S.string.with(S.meta, { errorMessage: { type: "Give me a string" } }), 1);
  expect(Object.keys(overridden.error!)).toContain("reason");
  expect(overridden.error!.reason).toBe("Give me a string");
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
