// The file is hand written

import { Error, Path, Schema } from "./S";

/* eslint-disable */
/* tslint:disable */

// genType fills these positionally from ReScript's `S.t<'value>`, whose single
// param is the output type — so the order stays output-first here even though
// `Schema` is input-first.
export type t<TOutput, TInput = unknown> = Schema<TInput, TOutput>;
export type schema<TOutput, TInput = unknown> = Schema<TInput, TOutput>;

export type Path_t = Path;

export type error = Error;
