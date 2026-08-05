// Types for the generated index.mjs (TS resolves .mjs imports to .d.mts).
// Shipped in the artifact too: that package is `type: "commonjs"`, so typing
// its ESM entry with the lone index.d.ts would hand node16 resolution a CJS
// declaration for an ESM file — hence the import condition's own `types`.
export * from "./index.js";
