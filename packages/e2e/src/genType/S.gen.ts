// FIXME: Shim so the relative `./S.gen` import that genType emits in
// GenType.gen.ts resolves to sury's hand-written type declarations.
//
// ReScript 12 genType computes a local path for modules from workspace
// (file:) dependencies instead of using the package-qualified path.
// Remove once genType emits `sury/src/S.gen` directly:
// https://github.com/rescript-lang/rescript/issues/8375
export type { error, t, schema, Path_t } from "sury/src/S.gen";
