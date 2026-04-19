// Shim so the relative `./S.gen` path emitted by genType resolves to the
// hand-written type declarations shipped with the sury package.
export type { error, t, schema, Path_t } from "sury/src/S.gen";
