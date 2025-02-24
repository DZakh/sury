let projectPath = "."
let artifactsPath = NodeJs.Path.join2(projectPath, "packages/artifacts")
let sourePaths = [
  "package.json",
  "node_modules",
  "src",
  "rescript.json",
  "README.md",
  "RescriptSchema.gen.d.ts",
]

module Stdlib = {
  module Json = {
    let rec update = (json, path, value) => {
      let dict = switch json->JSON.Decode.object {
      | Some(dict) => dict->Dict.copy
      | None => Dict.make()
      }
      switch path {
      | list{} => value
      | list{key} => {
          dict->Dict.set(key, value)
          dict->JSON.Encode.object
        }
      | list{key, ...path} => {
          dict->Dict.set(
            key,
            dict
            ->Dict.get(key)
            ->Option.getOr(Dict.make()->JSON.Encode.object)
            ->update(path, value),
          )
          dict->JSON.Encode.object
        }
      }
    }
  }
}

module Execa = {
  type returnValue = {stdout: string}
  type options = {env?: dict<string>, cwd?: string}

  @module("execa")
  external sync: (string, array<string>, ~options: options=?, unit) => returnValue = "execaSync"
}

module FsX = {
  type rmSyncOptions = {recursive?: bool, force?: bool}
  @module("fs") external rmSync: (string, rmSyncOptions) => unit = "rmSync"

  type cpSyncOptions = {recursive?: bool}
  @module("fs") external cpSync: (~src: string, ~dest: string, cpSyncOptions) => unit = "cpSync"
}

module Rollup = {
  type internalModuleFormat = [#amd | #cjs | #es | #iife | #system | #umd]
  type moduleFormat = [internalModuleFormat | #commonjs | #esm | #"module" | #systemjs]

  module Plugin = {
    type t
  }

  module NodeResolvePlugin = {
    @module("@rollup/plugin-node-resolve") external make: unit => Plugin.t = "nodeResolve"
  }

  module InputOptions = {
    type t = {
      input?: string,
      plugins?: array<Plugin.t>,
      @as("external")
      external_?: array<RegExp.t>,
    }
  }

  module OutputOptions = {
    type t = {
      // only needed for Bundle.write
      dir?: string,
      // only needed for Bundle.write
      file?: string,
      format?: moduleFormat,
      exports?: [#default | #named | #none | #auto],
      plugins?: array<Plugin.t>,
    }
  }

  module Output = {
    type t
  }

  module Bundle = {
    type t

    @module("rollup")
    external make: InputOptions.t => promise<t> = "rollup"

    @send
    external write: (t, OutputOptions.t) => promise<Output.t> = "write"

    @send
    external close: t => promise<unit> = "close"
  }
}

if NodeJs.Fs.existsSync(artifactsPath) {
  FsX.rmSync(artifactsPath, {recursive: true, force: true})
}
NodeJs.Fs.mkdirSync(artifactsPath)

let filesMapping = [
  ("Error", "S.$$Error.$$class"),
  ("string", "S.string"),
  ("boolean", "S.bool"),
  ("int32", "S.int"),
  ("number", "S.float"),
  ("bigint", "S.bigint"),
  ("json", "S.json"),
  ("never", "S.never"),
  ("unknown", "S.unknown"),
  ("undefined", "S.unit"),
  ("optional", "S.js_optional"),
  ("nullable", "S.$$null"),
  ("nullish", "S.nullable"),
  ("array", "S.array"),
  ("unnest", "S.unnest"),
  ("record", "S.dict"),
  ("jsonString", "S.jsonString"),
  ("union", "S.js_union"),
  ("object", "S.object"),
  ("schema", "S.js_schema"),
  ("safe", "S.js_safe"),
  ("safeAsync", "S.js_safeAsync"),
  ("reverse", "S.reverse"),
  ("convertOrThrow", "S.convertOrThrow"),
  ("convertToJsonOrThrow", "S.convertToJsonOrThrow"),
  ("convertToJsonStringOrThrow", "S.convertToJsonStringOrThrow"),
  ("reverseConvertOrThrow", "S.reverseConvertOrThrow"),
  ("reverseConvertToJsonOrThrow", "S.reverseConvertToJsonOrThrow"),
  ("reverseConvertToJsonStringOrThrow", "  S.reverseConvertToJsonStringOrThrow"),
  ("parseOrThrow", "S.parseOrThrow"),
  ("parseJsonOrThrow", "S.parseJsonOrThrow"),
  ("parseJsonStringOrThrow", "S.parseJsonStringOrThrow"),
  ("parseAsyncOrThrow", "S.parseAsyncOrThrow"),
  ("assertOrThrow", "S.assertOrThrow"),
  ("recursive", "S.recursive"),
  ("merge", "S.js_merge"),
  ("strict", "S.strict"),
  ("deepStrict", "S.deepStrict"),
  ("strip", "S.strip"),
  ("deepStrip", "S.deepStrip"),
  ("custom", "S.js_custom"),
  ("standard", "S.standard"),
  ("coerce", "S.coerce"),
  ("shape", "S.shape"),
  ("tuple", "S.tuple"),
  ("asyncParserRefine", "S.js_asyncParserRefine"),
  ("refine", "S.js_refine"),
  ("transform", "S.js_transform"),
  ("description", "S.description"),
  ("describe", "S.describe"),
  ("name", "S.js_name"),
  ("setName", "S.setName"),
  ("removeTypeValidation", "S.removeTypeValidation"),
  ("compile", "S.compile"),
  ("port", "S.port"),
  ("numberMin", "S.floatMin"),
  ("numberMax", "S.floatMax"),
  ("arrayMinLength", "S.arrayMinLength"),
  ("arrayMaxLength", "S.arrayMaxLength"),
  ("arrayLength", "S.arrayLength"),
  ("stringMinLength", "S.stringMinLength"),
  ("stringMaxLength", "S.stringMaxLength"),
  ("stringLength", "S.stringLength"),
  ("email", "S.email"),
  ("uuid", "S.uuid"),
  ("cuid", "S.cuid"),
  ("url", "S.url"),
  ("pattern", "S.pattern"),
  ("datetime", "S.datetime"),
  ("trim", "S.trim"),
  ("setGlobalConfig", "S.setGlobalConfig"),
]

sourePaths->Array.forEach(path => {
  FsX.cpSync(
    ~src=NodeJs.Path.join2(projectPath, path),
    ~dest=NodeJs.Path.join2(artifactsPath, path),
    {recursive: true},
  )
})

let writeSjsEsm = path => {
  NodeJs.Fs.writeFileSyncWith(
    path,
    ["import * as S from \"./S_Core.res.mjs\";"]
    ->Array.concat(filesMapping->Array.map(((name, value)) => `export const ${name} = ${value}`))
    ->Array.join("\n")
    ->NodeJs.Buffer.fromString,
    {
      encoding: "utf8",
    },
  )
}

// Sync the original source as well. Call it S.js to make .d.ts resolve correctly
writeSjsEsm("./src/S.js")

writeSjsEsm(NodeJs.Path.join2(artifactsPath, "./src/S.mjs"))

// This should overwrite S.js with the commonjs version
NodeJs.Fs.writeFileSyncWith(
  NodeJs.Path.join2(artifactsPath, "./src/S.js"),
  ["var S = require(\"./S_Core.res.js\");"]
  ->Array.concat(filesMapping->Array.map(((name, value)) => `exports.${name} = ${value}`))
  ->Array.join("\n")
  ->NodeJs.Buffer.fromString,
  {
    encoding: "utf8",
  },
)

let updateJsonFile = (~src, ~path, ~value) => {
  let packageJsonData = NodeJs.Fs.readFileSyncWith(
    src,
    {
      encoding: "utf8",
    },
  )
  let packageJson = packageJsonData->NodeJs.Buffer.toString->JSON.parseExn
  let updatedPackageJson =
    packageJson->Stdlib.Json.update(path->List.fromArray, value)->JSON.stringify(~space=2)
  NodeJs.Fs.writeFileSyncWith(
    src,
    updatedPackageJson->NodeJs.Buffer.fromString,
    {
      encoding: "utf8",
    },
  )
}

let _ = Execa.sync("npm", ["run", "res:build"], ~options={cwd: artifactsPath}, ())

let resolveRescriptRuntime = async (~format, ~input, ~output) => {
  let bundle = await Rollup.Bundle.make({
    input: NodeJs.Path.join2(artifactsPath, input),
    plugins: [Rollup.NodeResolvePlugin.make()],
  })
  let _ = await bundle->Rollup.Bundle.write({
    file: NodeJs.Path.join2(artifactsPath, output),
    format,
    exports: #named,
  })
  await bundle->Rollup.Bundle.close
}

// Inline "rescript" runtime dependencies,
// so it's not required for JS/TS to install ReScript compiler
// And if the package is used together by TS and ReScript,
// the file will be overwritten by compiler and share the same code
await resolveRescriptRuntime(~format=#es, ~input="src/S_Core.res.mjs", ~output="src/S_Core.res.mjs")
// Event though the generated code is shitty, let's still have it for the sake of some users
await resolveRescriptRuntime(~format=#cjs, ~input="src/S_Core.res.mjs", ~output="src/S_Core.res.js")
// Also build cjs version, in case some ReScript libraries will use rescript-schema without running a compiler (rescript-stdlib-vendorer)
await resolveRescriptRuntime(~format=#cjs, ~input="src/S.res.mjs", ~output="src/S.res.js")

// ReScript applications don't work with type: module set on packages
updateJsonFile(
  ~src=NodeJs.Path.join2(artifactsPath, "package.json"),
  ~path=["type"],
  ~value=JSON.Encode.string("commonjs"),
)
updateJsonFile(
  ~src=NodeJs.Path.join2(artifactsPath, "package.json"),
  ~path=["private"],
  ~value=JSON.Encode.bool(false),
)

// Clean up before uploading artifacts
FsX.rmSync(NodeJs.Path.join2(artifactsPath, "lib"), {force: true, recursive: true})
FsX.rmSync(NodeJs.Path.join2(artifactsPath, "node_modules"), {force: true, recursive: true})
