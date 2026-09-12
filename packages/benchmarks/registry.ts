// The registry every other module reads: what pages exist, in the order the
// nav line lists them.
//
// A topic is data plus four measuring functions. Splitting them apart is what
// lets a pull request check the three that a machine cannot influence while
// leaving the timings to the push that lands on main.
import type { Table } from "./table";

export type Source = {
  id: string;
  title: string;
  label: string;
  blurb: string;
  versions: () => string[];
  bundleSize: () => Promise<Table>;
  features: () => Promise<Table>;
  conformance: () => Promise<Table>;
  performance: () => Promise<Table>;
};

import { jsonSchema } from "./topics/jsonSchema";
import { jsonString } from "./topics/jsonString";
import { protobuf } from "./topics/protobuf";
import { schema } from "./topics/schema";

export const SOURCES: Source[] = [schema, jsonString, jsonSchema, protobuf];
