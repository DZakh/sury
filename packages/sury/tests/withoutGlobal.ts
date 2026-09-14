import { execFileSync } from "node:child_process";
import { fileURLToPath } from "node:url";

// `S.blob`, `S.file` and `S.formData` bind their class at import, so the
// runtime-missing case can only be observed in a process that never had the
// global - which is why those are tests and not specs. Booting Node and
// importing the bundle is ~60ms, so `routes` runs every route in one child
// rather than one child each.
const run = (name: string, body: string): string =>
  execFileSync(
    process.execPath,
    [
      "--input-type=module",
      "-e",
      // `File`, `Blob` and `FormData` are one internal undici module, lazily
      // loaded on first touch since node 24, and its initializer reads the
      // global `File`. Deleting one and then touching another loads undici
      // with that binding already gone (`ReferenceError: File is not
      // defined`). Reading all three first finishes the load while they are
      // all still there, so the delete takes only what Sury reads.
      `void [globalThis.File, globalThis.Blob, globalThis.FormData];
       delete globalThis.${name};
       const S = await import(${JSON.stringify(fileURLToPath(new URL("../index.mjs", import.meta.url)))});
       ${body}`,
    ],
    { encoding: "utf8" },
  ).trim();

export const withoutGlobal = run;

// Each route's own line: the message it reported, or `ok:<result>` when it
// didn't throw. A route that prints nothing would collapse two lines into one,
// so every branch prints.
export const withoutGlobalRoutes = (name: string, routes: string[]): string[] =>
  run(
    name,
    routes
      .map(
        (route) =>
          `try { console.log("ok:" + (${route})) } catch (e) { console.log(e.message) }`,
      )
      .join("\n"),
  ).split("\n");
