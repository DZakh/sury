import typia from "typia";

export interface User { id: number; name: string; email: string; age: number; verified: boolean; score: number; role: string }
export const encUser = (v: User) => typia.json.assertStringify<User>(v);

export interface Row { id: number; name: string; active: boolean }
export const encRows = (v: Row[]) => typia.json.assertStringify<Row[]>(v);

export type Ev =
  | { type: "click"; x: number; y: number }
  | { type: "view"; path: string }
  | { type: "error"; message: string; code: number };
export interface Feed { events: Ev[] }
export const encFeed = (v: Feed) => typia.json.assertStringify<Feed>(v);

export const encNumDict = (v: Record<string, number>) => typia.json.assertStringify<Record<string, number>>(v);
export const encStrDict = (v: Record<string, string>) => typia.json.assertStringify<Record<string, string>>(v);

export interface WireEvent { id: string; payload: string; createdAt: string; label: string }
export const encWire = (v: WireEvent) => typia.json.assertStringify<WireEvent>(v);
