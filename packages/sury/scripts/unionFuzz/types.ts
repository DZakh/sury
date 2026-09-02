export type Sury = Record<string, any>;

export type Outcome =
  | { ok: true; value: string }
  | { ok: false; kind: "sury"; message: string; reasons: number }
  | { ok: false; kind: "foreign"; name: string; message: string };

export type DiffClass = "acceptance" | "exception-kind" | "reasons" | "message";

export const NO_WITNESS = Symbol("union-fuzz-no-witness");
