import { expectTypeOf, test } from "vitest";

import * as S from "sury";
import * as GenType from "./GenType.gen";

test("genType emits sury's own schema and error types", () => {
  expectTypeOf(GenType.stringSchema).toEqualTypeOf<S.Schema<unknown, string>>();
  expectTypeOf(GenType.error).toEqualTypeOf<S.Error>();
});
