# AGENTS.md

This file provides guidance to coding agents working with code in this repository.

## Project Overview

rescript-schema is a high-performance validation/parsing library written in ReScript that also works with plain JavaScript and TypeScript. It uses JIT compilation via `eval` to generate optimized validators.

## Common Commands

```bash
# Install dependencies
pnpm i

# Build the main library (ReScript)
npm run res:build

# Watch mode for library
npm run res

# Compile tests (watch mode, run from root)
npm run test:res

# Run all tests (ava)
npm test

# Run a single test file
npx ava packages/tests/src/core/S_string_test.res.mjs

# Run tests matching a pattern
npx ava packages/tests/src/core/S_object*

# Test with coverage
npm run coverage

# Run benchmarks
npm run benchmark

# Lint stdlib vendoring
npm run lint:stdlib

# Build package for publishing
npm run build
```

## Development Workflow

1. Edit `.res` files in `src/` (library) or `packages/tests/src/` (tests)
2. ReScript compiler produces `.res.mjs` files in-source
3. Tests run against the compiled `.res.mjs` files via ava
4. Typical flow: run `npm run res` and `npm run test:res` in separate terminals, then `npm test`

## Architecture

**Core library** lives in `src/`:
- `S_Core.res` (~5k lines) — the entire schema implementation: types, builders, parsing, serialization, error handling
- `S_Core.resi` — interface file (public + internal API)
- `S.res` / `S.resi` — thin re-export layer exposing only the public API
- `S.d.ts` — hand-maintained TypeScript definitions for JS/TS consumers

**Key design**: Schemas are compiled into optimized JS functions via string-based code generation (`eval`). The `flag` system uses bitwise operations to control compilation modes (parse, serialize, assert, async, JSON).

**Test suite** in `packages/tests/src/core/`:
- Each test file covers one schema function or feature (e.g., `S_string_test.res`, `S_object_test.res`)
- Tests use `@dzakh/rescript-ava` bindings
- Test utilities in `packages/tests/src/utils/U.res` provide helpers for error assertions and compiled code snapshots
- Tests open `RescriptSchema` namespace automatically (configured in `packages/tests/rescript.json`)

**PPX** in `packages/rescript-schema-ppx/`:
- OCaml PPX (built with Dune) that generates schema definitions from `@schema` type annotations
- Built separately per platform (Linux, macOS, Windows)

**Prepack** in `packages/prepack/`:
- Build script that bundles the library for npm publishing using Rollup

## Monorepo Structure

- Package manager: pnpm with workspaces
- Root `rescript.json` compiles `src/` (library)
- `packages/tests/rescript.json` compiles tests with `-open RescriptSchema` and PPX flags
- Tests compile to `.res.mjs` (ESM, in-source)
