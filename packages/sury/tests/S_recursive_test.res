open Ava
open RescriptCore

type rec node = {
  id: string,
  children: array<node>,
}

test("Successfully parses recursive object", t => {
  let nodeSchema = S.recursive("Node", nodeSchema => {
    S.object(
      s => {
        id: s.field("Id", S.string),
        children: s.field("Children", S.array(nodeSchema)),
      },
    )
  })

  t->Assert.deepEqual(
    {
      "Id": "1",
      "Children": [
        {"Id": "2", "Children": []},
        {"Id": "3", "Children": [{"Id": "4", "Children": []}]},
      ],
    }->S.parseOrThrow(nodeSchema),
    {
      id: "1",
      children: [{id: "2", children: []}, {id: "3", children: [{id: "4", children: []}]}],
    },
  )
})

test("Fails to parses recursive object when provided invalid type", t => {
  let nodeSchema = S.recursive("Node", nodeSchema => {
    S.object(
      s => {
        id: s.field("Id", S.string),
        children: s.field("Children", S.array(nodeSchema)),
      },
    )
  })

  t->Assert.deepEqual(
    switch {
      "Id": "1",
      "Children": ["invalid"],
    }->S.parseOrThrow(nodeSchema) {
    | _ => "Shouldn't pass"
    | exception S.Error({message}) => message
    },
    `Failed parsing at ["Children"]["0"]: Expected Node, received "invalid"`,
  )
})

asyncTest("Successfully parses recursive object using S.parseAsyncOrThrow", t => {
  let nodeSchema = S.recursive("Node", nodeSchema => {
    S.object(
      s => {
        id: s.field("Id", S.string),
        children: s.field("Children", S.array(nodeSchema)),
      },
    )
  })

  t->U.assertCompiledCode(
    ~schema=nodeSchema,
    ~op=#ParseAsync,
    `i=>{let v0=e[0](i);return Promise.resolve(v0)}
Node: i=>{if(typeof i!=="object"||!i||!Array.isArray(i["Children"])){e[0](i)}let v0=i["Id"];if(typeof v0!=="string"){e[1](v0)}let v1=i["Children"],v6=new Array(v1.length);for(let v2=0;v2<v1.length;++v2){let v5;try{let v4=e[2][3](v1[v2]);v5=v4}catch(v3){if(v3&&v3.s===s){v3.path="[\\"Children\\"]"+'["'+v2+'"]'+v3.path}throw v3}v6[v2]=v5}return {"id":v0,"children":v6,}}`,
  )

  %raw(`{
    "Id": "1",
    "Children": [
      {"Id": "2", "Children": []},
      {"Id": "3", "Children": [{"Id": "4", "Children": []}]},
    ],
  }`)
  ->S.parseAsyncOrThrow(nodeSchema)
  ->Promise.thenResolve(result => {
    t->Assert.deepEqual(
      result,
      {
        id: "1",
        children: [{id: "2", children: []}, {id: "3", children: [{id: "4", children: []}]}],
      },
    )
  })
})

test("Successfully serializes recursive object", t => {
  let nodeSchema = S.recursive("Node", nodeSchema => {
    S.object(
      s => {
        id: s.field("Id", S.string),
        children: s.field("Children", S.array(nodeSchema)),
      },
    )
  })

  t->U.assertCompiledCode(
    ~schema=nodeSchema->S.reverse,
    ~op=#Convert,
    `i=>{let v0=e[0](i);return v0}
Node: i=>{let v0=i["children"],v5=new Array(v0.length);for(let v1=0;v1<v0.length;++v1){let v4;try{let v3=e[0][0](v0[v1]);v4=v3}catch(v2){if(v2&&v2.s===s){v2.path="[\\"children\\"]"+'["'+v1+'"]'+v2.path}throw v2}v5[v1]=v4}return {"Id":i["id"],"Children":v5,}}`,
  )

  t->Assert.deepEqual(
    {
      id: "1",
      children: [{id: "2", children: []}, {id: "3", children: [{id: "4", children: []}]}],
    }->S.reverseConvertOrThrow(nodeSchema),
    %raw(`{
        "Id": "1",
        "Children": [
          {"Id": "2", "Children": []},
          {"Id": "3", "Children": [{"Id": "4", "Children": []}]},
        ],
      }`),
  )
})

test("Fails to parse nested recursive object", t => {
  let nodeSchema = S.recursive("Node", nodeSchema => {
    S.object(
      s => {
        id: s.field(
          "Id",
          S.string->S.refine(
            s => id => {
              if id === "4" {
                s.fail("Invalid id")
              }
            },
          ),
        ),
        children: s.field("Children", S.array(nodeSchema)),
      },
    )
  })

  t->U.assertThrows(
    () =>
      {
        "Id": "1",
        "Children": [
          {"Id": "2", "Children": []},
          {"Id": "3", "Children": [{"Id": "4", "Children": []}]},
        ],
      }->S.parseOrThrow(nodeSchema),
    {
      code: OperationFailed("Invalid id"),
      operation: Parse,
      path: S.Path.fromArray(["Children", "1", "Children", "0", "Id"]),
    },
  )
})

test("Fails to parse nested recursive object inside of another object", t => {
  let schema = S.object(s =>
    s.field(
      "recursive",
      S.recursive(
        "Node",
        nodeSchema => {
          S.object(
            s => {
              id: s.field(
                "Id",
                S.string->S.refine(
                  s => id => {
                    if id === "4" {
                      s.fail("Invalid id")
                    }
                  },
                ),
              ),
              children: s.field("Children", S.array(nodeSchema)),
            },
          )
        },
      ),
    )
  )

  t->U.assertThrows(
    () =>
      {
        "recursive": {
          "Id": "1",
          "Children": [
            {"Id": "2", "Children": []},
            {"Id": "3", "Children": [{"Id": "4", "Children": []}]},
          ],
        },
      }->S.parseOrThrow(schema),
    {
      code: OperationFailed("Invalid id"),
      operation: Parse,
      path: S.Path.fromArray(["recursive", "Children", "1", "Children", "0", "Id"]),
    },
  )
})

test("Parses multiple nested recursive object inside of another object", t => {
  let schema = S.object(s =>
    {
      "recursive1": s.field(
        "recursive1",
        S.recursive(
          "Node",
          nodeSchema => {
            S.object(
              s => {
                id: s.field("Id", S.string),
                children: s.field("Children", S.array(nodeSchema)),
              },
            )
          },
        ),
      ),
      "recursive2": s.field(
        "recursive2",
        S.recursive(
          "Node",
          nodeSchema => {
            S.object(
              s => {
                id: s.field("Id", S.string),
                children: s.field("Children", S.array(nodeSchema)),
              },
            )
          },
        ),
      ),
    }
  )

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="object"||!i){e[0](i)}let v1;try{v1=e[1](i["recursive1"])}catch(v0){if(v0&&v0.s===s){v0.path="[\\"recursive1\\"]"+v0.path}throw v0}let v3;try{v3=e[2](i["recursive2"])}catch(v2){if(v2&&v2.s===s){v2.path="[\\"recursive2\\"]"+v2.path}throw v2}return {"recursive1":v1,"recursive2":v3,}}`,
  )

  t->Assert.deepEqual(
    {
      "recursive1": {
        "Id": "1",
        "Children": [
          {"Id": "2", "Children": []},
          {"Id": "3", "Children": [{"Id": "4", "Children": []}]},
        ],
      },
      "recursive2": {
        "Id": "1",
        "Children": [
          {"Id": "2", "Children": []},
          {"Id": "3", "Children": [{"Id": "4", "Children": []}]},
        ],
      },
    }->S.parseOrThrow(schema),
    {
      "recursive1": {
        id: "1",
        children: [{id: "2", children: []}, {id: "3", children: [{id: "4", children: []}]}],
      },
      "recursive2": {
        id: "1",
        children: [{id: "2", children: []}, {id: "3", children: [{id: "4", children: []}]}],
      },
    },
  )
})

test("Fails to serialise nested recursive object", t => {
  let nodeSchema = S.recursive("Node", nodeSchema => {
    S.object(
      s => {
        id: s.field(
          "Id",
          S.string->S.refine(
            s => id => {
              if id === "4" {
                s.fail("Invalid id")
              }
            },
          ),
        ),
        children: s.field("Children", S.array(nodeSchema)),
      },
    )
  })

  t->U.assertThrows(
    () =>
      {
        id: "1",
        children: [{id: "2", children: []}, {id: "3", children: [{id: "4", children: []}]}],
      }->S.reverseConvertOrThrow(nodeSchema),
    {
      code: OperationFailed("Invalid id"),
      operation: ReverseConvert,
      path: S.Path.fromArray(["children", "1", "children", "0", "id"]),
    },
  )
})

test(
  "Recursively transforms all objects when added transform to the recursive's function returned schema",
  t => {
    let nodeSchema = S.recursive("Node", nodeSchema => {
      S.object(
        s => {
          id: s.field("Id", S.string),
          children: s.field("Children", S.array(nodeSchema)),
        },
      )->S.transform(
        _ => {
          parser: node => {...node, id: `node_${node.id}`},
          serializer: node => {...node, id: node.id->String.sliceToEnd(~start=5)},
        },
      )
    })

    t->U.assertCompiledCode(
      ~schema=nodeSchema,
      ~op=#Parse,
      `i=>{let v0=e[0](i);return v0}
Node: i=>{if(typeof i!=="object"||!i||!Array.isArray(i["Children"])){e[0](i)}let v0=i["Id"];if(typeof v0!=="string"){e[1](v0)}let v1=i["Children"],v6=new Array(v1.length);for(let v2=0;v2<v1.length;++v2){let v5;try{let v4=e[2][1](v1[v2]);v5=v4}catch(v3){if(v3&&v3.s===s){v3.path="[\\"Children\\"]"+'["'+v2+'"]'+v3.path}throw v3}v6[v2]=v5}return e[3]({"id":v0,"children":v6,})}`,
    )
    t->Assert.deepEqual(
      {
        "Id": "1",
        "Children": [
          {"Id": "2", "Children": []},
          {"Id": "3", "Children": [{"Id": "4", "Children": []}]},
        ],
      }->S.parseOrThrow(nodeSchema),
      {
        id: "node_1",
        children: [
          {id: "node_2", children: []},
          {id: "node_3", children: [{id: "node_4", children: []}]},
        ],
      },
    )

    t->U.assertCompiledCode(
      ~schema=nodeSchema,
      ~op=#ReverseConvert,
      `i=>{let v0=e[0](i);return v0}`,
    )
    t->Assert.deepEqual(
      {
        id: "node_1",
        children: [
          {id: "node_2", children: []},
          {id: "node_3", children: [{id: "node_4", children: []}]},
        ],
      }->S.reverseConvertOrThrow(nodeSchema),
      {
        "Id": "1",
        "Children": [
          {"Id": "2", "Children": []},
          {"Id": "3", "Children": [{"Id": "4", "Children": []}]},
        ],
      }->Obj.magic,
    )
  },
)

test("Recursively transforms nested objects when added transform to the placeholder schema", t => {
  let nodeSchema = S.recursive("Node", nodeSchema => {
    S.object(
      s => {
        id: s.field("Id", S.string),
        children: s.field(
          "Children",
          S.array(
            nodeSchema->S.transform(
              _ => {
                parser: node => {...node, id: `child_${node.id}`},
                serializer: node => {...node, id: node.id->String.sliceToEnd(~start=6)},
              },
            ),
          ),
        ),
      },
    )
  })

  t->Assert.deepEqual(
    {
      "Id": "1",
      "Children": [
        {"Id": "2", "Children": []},
        {"Id": "3", "Children": [{"Id": "4", "Children": []}]},
      ],
    }->S.parseOrThrow(nodeSchema),
    {
      id: "1",
      children: [
        {id: "child_2", children: []},
        {id: "child_3", children: [{id: "child_4", children: []}]},
      ],
    },
  )
  t->Assert.deepEqual(
    {
      id: "1",
      children: [
        {id: "child_2", children: []},
        {id: "child_3", children: [{id: "child_4", children: []}]},
      ],
    }->S.reverseConvertOrThrow(nodeSchema),
    {
      "Id": "1",
      "Children": [
        {"Id": "2", "Children": []},
        {"Id": "3", "Children": [{"Id": "4", "Children": []}]},
      ],
    }->Obj.magic,
  )
})

test("Shallowly transforms object when added transform to the S.recursive result", t => {
  let nodeSchema = S.recursive("Node", nodeSchema => {
    S.object(
      s => {
        id: s.field("Id", S.string),
        children: s.field("Children", S.array(nodeSchema)),
      },
    )
  })->S.transform(_ => {
    parser: node => {...node, id: `parent_${node.id}`},
    serializer: node => {...node, id: node.id->String.sliceToEnd(~start=7)},
  })

  t->Assert.deepEqual(
    {
      "Id": "1",
      "Children": [
        {"Id": "2", "Children": []},
        {"Id": "3", "Children": [{"Id": "4", "Children": []}]},
      ],
    }->S.parseOrThrow(nodeSchema),
    {
      id: "parent_1",
      children: [{id: "2", children: []}, {id: "3", children: [{id: "4", children: []}]}],
    },
  )
  t->Assert.deepEqual(
    {
      id: "parent_1",
      children: [{id: "2", children: []}, {id: "3", children: [{id: "4", children: []}]}],
    }->S.reverseConvertOrThrow(nodeSchema),
    {
      "Id": "1",
      "Children": [
        {"Id": "2", "Children": []},
        {"Id": "3", "Children": [{"Id": "4", "Children": []}]},
      ],
    }->Obj.magic,
  )

  t->U.assertCompiledCode(
    ~schema=nodeSchema,
    ~op=#Parse,
    `i=>{let v0=e[0](i);return e[1](v0)}
Node: i=>{if(typeof i!=="object"||!i||!Array.isArray(i["Children"])){e[0](i)}let v0=i["Id"];if(typeof v0!=="string"){e[1](v0)}let v1=i["Children"],v6=new Array(v1.length);for(let v2=0;v2<v1.length;++v2){let v5;try{let v4=e[2][1](v1[v2]);v5=v4}catch(v3){if(v3&&v3.s===s){v3.path="[\\"Children\\"]"+'["'+v2+'"]'+v3.path}throw v3}v6[v2]=v5}return {"id":v0,"children":v6,}}`,
  )
  t->U.assertCompiledCode(
    ~schema=nodeSchema,
    ~op=#ReverseConvert,
    `i=>{let v0=e[1](e[0](i));return v0}`,
  )
})

asyncTest("Successfully parses recursive object with async parse function", t => {
  let nodeSchema = S.recursive("Node", nodeSchema => {
    S.object(
      s => {
        id: s.field("Id", S.string->S.transform(_ => {asyncParser: i => Promise.resolve(i)})),
        children: s.field("Children", S.array(nodeSchema)),
      },
    )
  })

  t->U.assertCompiledCode(
    ~schema=nodeSchema,
    ~op=#ParseAsync,
    `i=>{let v0=e[0](i);return v0}
Node: i=>{if(typeof i!=="object"||!i||!Array.isArray(i["Children"])){e[0](i)}let v0=i["Id"];if(typeof v0!=="string"){e[1](v0)}let v1=i["Children"],v6=new Array(v1.length);for(let v2=0;v2<v1.length;++v2){let v5;try{let v4=e[3][3](v1[v2]);v5=v4.catch(v3=>{if(v3&&v3.s===s){v3.path="[\\"Children\\"]"+'["'+v2+'"]'+v3.path}throw v3})}catch(v3){if(v3&&v3.s===s){v3.path="[\\"Children\\"]"+'["'+v2+'"]'+v3.path}throw v3}v6[v2]=v5}return Promise.all([e[2](v0),Promise.all(v6),]).then(a=>({"id":a[0],"children":a[1],}))}`,
  )

  %raw(`{
    "Id":"1",
    "Children": [
      {"Id": "2", "Children": []},
      {"Id": "3", "Children": [{"Id": "4", "Children": []}]},
    ],
  }`)
  ->S.parseAsyncOrThrow(nodeSchema)
  ->Promise.thenResolve(result => {
    t->Assert.deepEqual(
      result,
      {
        id: "1",
        children: [{id: "2", children: []}, {id: "3", children: [{id: "4", children: []}]}],
      },
    )
  })
})

test("Parses recursive object with async fields in parallel", t => {
  let unresolvedPromise = Promise.make((_, _) => ())
  let actionCounter = ref(0)

  let nodeSchema = S.recursive("Node", nodeSchema => {
    S.object(
      s => {
        id: s.field(
          "Id",
          S.string->S.transform(
            _ => {
              asyncParser: _ => {
                actionCounter.contents = actionCounter.contents + 1
                unresolvedPromise
              },
            },
          ),
        ),
        children: s.field("Children", S.array(nodeSchema)),
      },
    )
  })

  %raw(`{
    "Id": "1",
    "Children": [
      {"Id": "2", "Children": []},
      {"Id": "3", "Children": [{"Id": "4", "Children": []}]},
    ],
  }`)
  ->S.parseAsyncOrThrow(nodeSchema)
  ->ignore

  t->Assert.deepEqual(actionCounter.contents, 4)

  t->U.assertCompiledCode(
    ~schema=nodeSchema,
    ~op=#ParseAsync,
    `i=>{let v0=e[0](i);return v0}
Node: i=>{if(typeof i!=="object"||!i||!Array.isArray(i["Children"])){e[0](i)}let v0=i["Id"];if(typeof v0!=="string"){e[1](v0)}let v1=i["Children"],v6=new Array(v1.length);for(let v2=0;v2<v1.length;++v2){let v5;try{let v4=e[3][3](v1[v2]);v5=v4.catch(v3=>{if(v3&&v3.s===s){v3.path="[\\"Children\\"]"+'["'+v2+'"]'+v3.path}throw v3})}catch(v3){if(v3&&v3.s===s){v3.path="[\\"Children\\"]"+'["'+v2+'"]'+v3.path}throw v3}v6[v2]=v5}return Promise.all([e[2](v0),Promise.all(v6),]).then(a=>({"id":a[0],"children":a[1],}))}`,
  )
})

test("Compiled parse code snapshot", t => {
  let schema = S.recursive("Node", schema => {
    S.object(
      s => {
        id: s.field("Id", S.string),
        children: s.field("Children", S.array(schema)),
      },
    )
  })

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{let v0=e[0](i);return v0}
Node: i=>{if(typeof i!=="object"||!i||!Array.isArray(i["Children"])){e[0](i)}let v0=i["Id"];if(typeof v0!=="string"){e[1](v0)}let v1=i["Children"],v6=new Array(v1.length);for(let v2=0;v2<v1.length;++v2){let v5;try{let v4=e[2][1](v1[v2]);v5=v4}catch(v3){if(v3&&v3.s===s){v3.path="[\\"Children\\"]"+'["'+v2+'"]'+v3.path}throw v3}v6[v2]=v5}return {"id":v0,"children":v6,}}`,
  )
})
