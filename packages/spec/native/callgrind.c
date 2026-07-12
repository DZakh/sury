// Minimal Node-API addon exposing Valgrind/Callgrind client requests so the
// spec harness can fence exactly one schema build (or one operation compile)
// and read back its retired-instruction count.
//
// The macros come from the vendored callgrind.h/valgrind.h (BSD-licensed,
// designed to be embedded): they expand to a tiny inline-asm sequence that is
// a NO-OP unless the process runs under `valgrind --tool=callgrind`. So this
// builds and loads anywhere with just a C compiler + Node headers, and only
// does anything when the perf worker is launched under valgrind.
#include <node_api.h>
#include "callgrind.h"

// start(): reset counters, then begin counting. ZERO_STATS makes each fenced
// region report only its own instructions, independent of prior regions.
static napi_value StartRegion(napi_env env, napi_callback_info info) {
  CALLGRIND_ZERO_STATS;
  CALLGRIND_START_INSTRUMENTATION;
  return NULL;
}

// stop(name): stop counting and dump the accumulated stats under a trigger
// named `name`, which valgrind writes to a separate callgrind.out.<pid>.<n>
// file. perf.ts reads each dump's `totals:` line back, keyed by that name.
static napi_value StopRegion(napi_env env, napi_callback_info info) {
  size_t argc = 1;
  napi_value argv[1];
  napi_get_cb_info(env, info, &argc, argv, NULL, NULL);
  char name[256];
  size_t len = 0;
  if (argc >= 1) napi_get_value_string_utf8(env, argv[0], name, sizeof(name), &len);
  else name[0] = 0;
  CALLGRIND_STOP_INSTRUMENTATION;
  CALLGRIND_DUMP_STATS_AT(name);
  return NULL;
}

static napi_value Init(napi_env env, napi_value exports) {
  napi_value start, stop;
  napi_create_function(env, "start", NAPI_AUTO_LENGTH, StartRegion, NULL, &start);
  napi_create_function(env, "stop", NAPI_AUTO_LENGTH, StopRegion, NULL, &stop);
  napi_set_named_property(env, exports, "start", start);
  napi_set_named_property(env, exports, "stop", stop);
  return exports;
}

NAPI_MODULE(NODE_GYP_MODULE_NAME, Init)
