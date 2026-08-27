import type { Sury } from "./types";

const fail = (S: Sury, reason: string): never => {
  throw Object.assign(new S.Error(), {
    code: "invalid_operation",
    path: "",
    reason,
  });
};

export const issue347Schema = (S: Sury): unknown => {
  const taggedArm = S.json.with(S.to, S.any, {
    decode: (json: any) => {
      if (json && typeof json === "object" && "$ref" in json) {
        return { TAG: "Tagged", _0: json.$ref };
      }
      fail(S, "not tagged");
    },
    encode: (p: any) => {
      if (p.TAG === "Tagged") return { $ref: p._0 };
      fail(S, "not tagged");
    },
  });
  const plainArm = S.schema({ name: S.string }).with(S.to, S.any, {
    decode: (v: any) => ({ TAG: "Plain", _0: v }),
    encode: (p: any) => {
      if (p.TAG === "Plain") return p._0;
      fail(S, "not plain");
    },
  });
  return S.nullable(S.union([taggedArm, plainArm]));
};

export const issue347OptionVoidLastSchema = (S: Sury): unknown => {
  const taggedArm = S.json.with(S.to, S.any, {
    decode: (json: any) => {
      if (json && typeof json === "object" && "$ref" in json) {
        return { TAG: "Tagged", _0: json.$ref };
      }
      fail(S, "not tagged");
    },
    encode: (p: any) => {
      if (p.TAG === "Tagged") return { $ref: p._0 };
      fail(S, "not tagged");
    },
  });
  return S.$option(S.union([taggedArm, S.void]));
};
