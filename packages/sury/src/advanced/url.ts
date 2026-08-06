// `S.url` — a URI string on the JSON side, a `URL` on ours.
import {
  baseSchema,
  flagUnsafeHas,
  initSchema,
  instanceTag,
  type Internal,
  stringTag,
  tagFlagInstance,
  tagFlags,
  tagFlagString,
  tagFlagUnknown,
  type Val,
} from "../base";
import {
  B_embed,
  B_next,
  B_refine,
  B_unsupportedDecode,
  failInvalidType,
} from "../builder";
import { instanceDecoder, parse } from "../parse";
export const url: Internal = /* @__PURE__ */ initSchema(instanceTag, (s) => {
  // `new URL(…)` throws where `new Date(…)` merely yields an Invalid Date, so
  // the conversion goes through a helper — a thrown TypeError would escape as
  // itself rather than as a Sury error, and `URL.canParse` would parse twice.
  // It hands back the input string on failure rather than undefined so the
  // refine below reports the value the user actually passed.
  const urlFromString = (value: string) => {
    try {
      return new URL(value);
    } catch {
      return value;
    }
  };
  s.class = URL;
  s.decoder = (input: Val): Val => {
    const inputTagFlag = tagFlags[input.s.type]!;
    if (flagUnsafeHas(inputTagFlag, tagFlagString)) {
      return B_refine(
        B_next(input, `${B_embed(input, urlFromString)}(${input.i})`, s),
        input.e,
        [
          {
            c: (inputVar) => `typeof ${inputVar}!=="string"`,
            f: failInvalidType,
          },
        ],
      );
    } else if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
      return instanceDecoder(input);
    } else if (flagUnsafeHas(inputTagFlag, tagFlagInstance) && input.s.class === s.class) {
      return input;
    } else {
      return B_unsupportedDecode(input, input.s, input.e);
    }
  };
  s.encoder = (input, target) => {
    const toTagFlag = tagFlags[target.type]!;
    if (flagUnsafeHas(toTagFlag, tagFlagString)) {
      // Annotation, not a check: `.href` leaves `|` and `^` unescaped, the two
      // characters RFC 3986 forbids that survive `new URL`, so a href can be
      // one `S.uri` would reject. Validating here would drag the whole uri
      // regex into every `url` bundle to catch two characters, and would throw
      // on a URL the caller legitimately built.
      const uriString = baseSchema(stringTag, false);
      uriString.format = "uri";
      return parse(B_next(input, `${input.i}.href`, uriString, target));
    } else {
      return input;
    }
  };
});
