import { expect, test } from "vitest";
import * as S from "../index.mjs";
import {
  FUZZ_EXPORTS,
  missingCatalogNames,
  staleCatalogNames,
} from "../scripts/unionFuzz/catalog";

test("every public export has a fuzz classification", () => {
  expect(missingCatalogNames(S), "add a FUZZ_EXPORTS entry for each new export").toEqual(
    [],
  );
});

test("no stale fuzz classifications", () => {
  expect(staleCatalogNames(S)).toEqual([]);
});

test("the catalog is keyed by export name", () => {
  expect(Object.keys(FUZZ_EXPORTS).sort()).toEqual(Object.keys(S).sort());
});

test("every skip classification has a reason", () => {
  const empty = Object.entries(FUZZ_EXPORTS).filter(
    ([, spec]) => spec.use === "skip" && !spec.reason,
  );
  expect(empty).toEqual([]);
});
