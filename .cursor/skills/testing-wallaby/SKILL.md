---
name: testing-wallaby
description: Debug and fix failing tests using Wallaby.js MCP tools. Use when investigating test failures, debugging test errors, inspecting runtime values, checking code coverage, or updating snapshots.
---

# Testing with Wallaby

Use Wallaby MCP tools as the primary way to work with tests. Fall back to terminal only if Wallaby is unavailable.

## Debug Workflow

### 1. Identify failures

- **Broad**: `wallaby_failingTests` — all failing tests in the project
- **By file**: `wallaby_failingTestsForFile` — failures related to a specific source or test file
- **By line**: `wallaby_failingTestsForFileAndLine` — failures covering a specific line

Each returns test name, test ID, errors with stack traces, runtime logs, and coverage percentage.

### 2. Narrow down with coverage

- `wallaby_coveredLinesForFile` — which lines a file has covered, optionally filtered by test ID
- `wallaby_coveredLinesForTest` — all files/lines a specific test covers

Use coverage to find the implementation code a failing test actually executes.

### 3. Inspect runtime values

- `wallaby_runtimeValues` — value of an expression at a file/line across all tests
- `wallaby_runtimeValuesByTest` — value of an expression at a file/line for a specific test

Required params: `file`, `line` (1-based, count blank lines), `lineContent`, `expression`. For `runtimeValuesByTest`, also `testId`.

### 4. Fix the code

Apply the fix based on evidence from steps 1-3.

### 5. Verify the fix

- `wallaby_testById` — re-check a specific test by its ID
- `wallaby_failingTests` — confirm no remaining failures

Iterate steps 3-5 until the test passes.

### 6. Update snapshots (if needed)

- `wallaby_updateTestSnapshots` — update snapshots for one test (by test ID)
- `wallaby_updateFileSnapshots` — update all snapshots in/covering a file
- `wallaby_updateProjectSnapshots` — update every snapshot in the project

## Discovery Tools

Use these when exploring tests, not just debugging failures:

- `wallaby_allTests` — list every test in the project
- `wallaby_allTestsForFile` — tests related to a specific file
- `wallaby_allTestsForFileAndLine` — tests covering a specific file and line

## Principles

- Always cite runtime values and coverage data to justify conclusions.
- Explain reasoning step by step.
- Keep iterating with updated Wallaby data until the test passes.
