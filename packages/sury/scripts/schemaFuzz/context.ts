// What one drawn schema hands each family of properties: the schema, both of
// its sides sampled, and the two ways a family says something went wrong.

export type Fn = (...args: unknown[]) => unknown;

export type Ctx = {
  S: Record<string, any>;
  id: string;
  schema: unknown;
  reversed: unknown;
  // Carries a conversion that throws information away (see `MemberSpec`), so
  // no property may ask it for a round trip.
  lossy: boolean;
  // Pairs of values the Input side accepts, and pairs the Output side accepts.
  // Each pair is the same slot built twice from one seed: equal by
  // construction, and never the same object.
  inputs: [unknown, unknown][];
  outputs: [unknown, unknown][];
  isInput: (value: unknown) => unknown;
  isOutput: (value: unknown) => unknown;
  report: (property: string, detail: string) => void;
  // Compiles an operation. A Sury refusal is the schema's own contract and
  // comes back `undefined`; any other throw is reported under `property`.
  compile: <T extends Fn>(property: string, build: () => unknown) => T | undefined;
  count: (what: string, n?: number) => void;
};

// A family: the properties it holds, and the cases known not to hold, keyed by
// a SUBSTRING of the finding's key with the reason written by hand.
export type Family = {
  check: (ctx: Ctx) => void;
  known: Record<string, string>;
};

export const reason = (error: unknown): string => (error as Error).message.split("\n")[0]!;
