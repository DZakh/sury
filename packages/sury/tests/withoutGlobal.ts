import { execFileSync } from "node:child_process";
import { fileURLToPath } from "node:url";

// `S.blob`, `S.file` and `S.formData` bind their class at import, so the
// runtime-missing case can only be observed in a process that never had the
// global — which is why those are tests and not specs. Booting Node and
// importing the bundle is ~60ms, so `routes` runs every route in one child
// rather than one child each.
const run = (name: string, body: string): string =>
  execFileSync(
    process.execPath,
    [
      "--input-type=module",
      "-e",
      `delete globalThis.${name};
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
