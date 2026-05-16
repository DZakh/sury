open Ava

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
    }->S.parseOrThrow(~to=nodeSchema),
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
    }->S.parseOrThrow(~to=nodeSchema) {
    | _ => "Shouldn't pass"
    | exception S.Exn({message}) => message
    },
    `Failed at ["Children"]["0"]: Expected { Id: string; Children: Node[]; }, received "invalid"`,
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
    `i=>{let v0;v0=e[0](i);return Promise.resolve(v0)}
Node: i=>{typeof i==="object"&&i||e[3](i);let v0=i["Id"],v1=i["Children"];typeof v0==="string"||e[0](v0);Array.isArray(v1)||e[2](v1);let v5=new Array(v1.length);for(let v2=0;v2<v1.length;++v2){try{let v3;v3=e[1]["unknown->Node--1"](v1[v2]);v5[v2]=v3}catch(v4){v4.path="[\\"Children\\"]"+'["'+v2+'"]'+v4.path;throw v4}}return {"id":v0,"children":v5,}}`,
  )

  %raw(`{
    "Id": "1",
    "Children": [
      {"Id": "2", "Children": []},
      {"Id": "3", "Children": [{"Id": "4", "Children": []}]},
    ],
  }`)
  ->S.parseAsyncOrThrow(~to=nodeSchema)
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

  let reversedDefs = (nodeSchema->S.reverse->S.untag).defs->Option.getUnsafe
  let nodeSeq = (reversedDefs->Dict.getUnsafe("Node")->S.untag).seq->Float.toString
  let recKey = `${nodeSeq}-${nodeSeq}--0`
  t->U.assertCompiledCode(
    ~schema=nodeSchema->S.reverse,
    ~op=#Convert,
    ~embedded=[("Node", 0)],
    `i=>{let v0;v0=e[0](i);return v0}
Node: i=>{let v0=i["children"];let v4=new Array(v0.length);for(let v1=0;v1<v0.length;++v1){try{let v2;v2=e[0]["${recKey}"](v0[v1]);v4[v1]=v2}catch(v3){v3.path="[\\"children\\"]"+'["'+v1+'"]'+v3.path;throw v3}}return {"Id":i["id"],"Children":v4,}}`,
  )

  t->Assert.deepEqual(
    {
      id: "1",
      children: [{id: "2", children: []}, {id: "3", children: [{id: "4", children: []}]}],
    }->S.decodeOrThrow(~from=nodeSchema, ~to=S.unknown),
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
          S.string->S.refine(id => id !== "4", ~error="Invalid id"),
        ),
        children: s.field("Children", S.array(nodeSchema)),
      },
    )
  })

  t->U.assertThrowsMessage(() =>
    {
      "Id": "1",
      "Children": [
        {"Id": "2", "Children": []},
        {"Id": "3", "Children": [{"Id": "4", "Children": []}]},
      ],
    }->S.parseOrThrow(~to=nodeSchema)
  , `Failed at ["Children"]["1"]["Children"]["0"]["Id"]: Invalid id`)
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
                S.string->S.refine(id => id !== "4", ~error="Invalid id"),
              ),
              children: s.field("Children", S.array(nodeSchema)),
            },
          )
        },
      ),
    )
  )

  t->U.assertThrowsMessage(() =>
    {
      "recursive": {
        "Id": "1",
        "Children": [
          {"Id": "2", "Children": []},
          {"Id": "3", "Children": [{"Id": "4", "Children": []}]},
        ],
      },
    }->S.parseOrThrow(~to=schema)
  , `Failed at ["recursive"]["Children"]["1"]["Children"]["0"]["Id"]: Invalid id`)
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
    `i=>{typeof i==="object"&&i||e[2](i);let v0;try{v0=e[0](i["recursive1"]);}catch(v1){v1.path="[\\"recursive1\\"]"+v1.path;throw v1}let v2;try{v2=e[1](i["recursive2"]);}catch(v3){v3.path="[\\"recursive2\\"]"+v3.path;throw v3}return {"recursive1":v0,"recursive2":v2,}}`,
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
    }->S.parseOrThrow(~to=schema),
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
          S.string->S.refine(id => id !== "4", ~error="Invalid id"),
        ),
        children: s.field("Children", S.array(nodeSchema)),
      },
    )
  })

  t->U.assertThrowsMessage(
    () =>
      {
        id: "1",
        children: [{id: "2", children: []}, {id: "3", children: [{id: "4", children: []}]}],
      }->S.decodeOrThrow(~from=nodeSchema, ~to=S.unknown),
    `Failed at ["children"]["1"]["children"]["0"]["id"]: Invalid id`,
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
          serializer: node => {...node, id: node.id->String.slice(~start=5)},
        },
      )
    })

    t->U.assertCompiledCode(
      ~schema=nodeSchema,
      ~op=#Parse,
      `i=>{let v0;v0=e[0](i);return v0}
Node: i=>{typeof i==="object"&&i||e[5](i);let v0=i["Id"],v1=i["Children"];typeof v0==="string"||e[0](v0);Array.isArray(v1)||e[2](v1);let v5=new Array(v1.length);for(let v2=0;v2<v1.length;++v2){try{let v3;v3=e[1]["unknown->Node--0"](v1[v2]);v5[v2]=v3}catch(v4){v4.path="[\\"Children\\"]"+'["'+v2+'"]'+v4.path;throw v4}}let v6;try{v6=e[3]({"id":v0,"children":v5,})}catch(x){e[4](x)}return v6}`,
    )
    t->Assert.deepEqual(
      {
        "Id": "1",
        "Children": [
          {"Id": "2", "Children": []},
          {"Id": "3", "Children": [{"Id": "4", "Children": []}]},
        ],
      }->S.parseOrThrow(~to=nodeSchema),
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
      ~op=#Encode,
      ~embedded=[("Node", 0)],
      `i=>{let v0;v0=e[0](i);return v0}
Node: i=>{let v0;try{v0=e[0](i)}catch(x){e[1](x)}typeof v0==="object"&&v0||e[5](v0);let v1=v0["id"],v2=v0["children"];typeof v1==="string"||e[2](v1);Array.isArray(v2)||e[4](v2);let v6=new Array(v2.length);for(let v3=0;v3<v2.length;++v3){try{let v4;v4=e[3](v2[v3]);v6[v3]=v4}catch(v5){v5.path="[\\"children\\"]"+'["'+v3+'"]'+v5.path;throw v5}}return {"Id":v1,"Children":v6,}}`,
    )
    t->Assert.deepEqual(
      {
        id: "node_1",
        children: [
          {id: "node_2", children: []},
          {id: "node_3", children: [{id: "node_4", children: []}]},
        ],
      }->S.decodeOrThrow(~from=nodeSchema, ~to=S.unknown),
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
                serializer: node => {...node, id: node.id->String.slice(~start=6)},
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
    }->S.parseOrThrow(~to=nodeSchema),
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
    }->S.decodeOrThrow(~from=nodeSchema, ~to=S.unknown),
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
    serializer: node => {...node, id: node.id->String.slice(~start=7)},
  })

  // FIXME: There's a double run of array decoder
  t->Assert.deepEqual(
    {
      "Id": "1",
      "Children": [
        {"Id": "2", "Children": []},
        {"Id": "3", "Children": [{"Id": "4", "Children": []}]},
      ],
    }->S.parseOrThrow(~to=nodeSchema),
    {
      id: "parent_1",
      children: [{id: "2", children: []}, {id: "3", children: [{id: "4", children: []}]}],
    },
  )
  t->Assert.deepEqual(
    {
      id: "parent_1",
      children: [{id: "2", children: []}, {id: "3", children: [{id: "4", children: []}]}],
    }->S.decodeOrThrow(~from=nodeSchema, ~to=S.unknown),
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
    `i=>{let v0;v0=e[0](i);let v1;try{v1=e[1](v0)}catch(x){e[2](x)}return v1}
Node: i=>{typeof i==="object"&&i||e[3](i);let v0=i["Id"],v1=i["Children"];typeof v0==="string"||e[0](v0);Array.isArray(v1)||e[2](v1);let v5=new Array(v1.length);for(let v2=0;v2<v1.length;++v2){try{let v3;v3=e[1]["unknown->Node--0"](v1[v2]);v5[v2]=v3}catch(v4){v4.path="[\\"Children\\"]"+'["'+v2+'"]'+v4.path;throw v4}}return {"id":v0,"children":v5,}}`,
  )
  let reversedDefs =
    ((nodeSchema->S.reverse->S.untag).to->Option.getUnsafe->S.untag).defs->Option.getUnsafe
  let recKey = `${(S.unknown->S.untag).seq->Float.toString}-${(reversedDefs->Dict.getUnsafe("Node")->S.untag).seq->Float.toString}--0`
  t->U.assertCompiledCode(
    ~schema=nodeSchema,
    ~op=#Encode,
    ~embedded=[("Node", 2)],
    `i=>{let v0;try{v0=e[0](i)}catch(x){e[1](x)}let v1;v1=e[2](v0);return v1}
Node: i=>{typeof i==="object"&&i||e[3](i);let v0=i["id"],v1=i["children"];typeof v0==="string"||e[0](v0);Array.isArray(v1)||e[2](v1);let v5=new Array(v1.length);for(let v2=0;v2<v1.length;++v2){try{let v3;v3=e[1]["${recKey}"](v1[v2]);v5[v2]=v3}catch(v4){v4.path="[\\"children\\"]"+'["'+v2+'"]'+v4.path;throw v4}}return {"Id":v0,"Children":v5,}}`,
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
    `i=>{let v0;v0=e[0](i);return v0}
Node: i=>{typeof i==="object"&&i||e[5](i);let v1=i["Id"],v2=i["Children"];typeof v1==="string"||e[2](v1);let v0;try{v0=e[0](i["Id"]).catch(x=>e[1](x))}catch(x){e[1](x)}Array.isArray(v2)||e[4](v2);let v6=new Array(v2.length);for(let v3=0;v3<v2.length;++v3){try{let v4;v4=e[3]["unknown->Node--1"](v2[v3]);v6[v3]=v4.catch(v5=>{v5.path="[\\"Children\\"]"+'["'+v3+'"]'+v5.path;throw v5})}catch(v5){v5.path="[\\"Children\\"]"+'["'+v3+'"]'+v5.path;throw v5}}let v7=Promise.all(v6);return Promise.all([v0,v7,]).then(([v0,v7,])=>{return {"id":v0,"children":v7,}})}`,
  )

  %raw(`{
    "Id":"1",
    "Children": [
      {"Id": "2", "Children": []},
      {"Id": "3", "Children": [{"Id": "4", "Children": []}]},
    ],
  }`)
  ->S.parseAsyncOrThrow(~to=nodeSchema)
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
  ->S.parseAsyncOrThrow(~to=nodeSchema)
  ->ignore

  t->Assert.deepEqual(actionCounter.contents, 4)

  t->U.assertCompiledCode(
    ~schema=nodeSchema,
    ~op=#ParseAsync,
    `i=>{let v0;v0=e[0](i);return v0}
Node: i=>{typeof i==="object"&&i||e[5](i);let v1=i["Id"],v2=i["Children"];typeof v1==="string"||e[2](v1);let v0;try{v0=e[0](i["Id"]).catch(x=>e[1](x))}catch(x){e[1](x)}Array.isArray(v2)||e[4](v2);let v6=new Array(v2.length);for(let v3=0;v3<v2.length;++v3){try{let v4;v4=e[3]["unknown->Node--1"](v2[v3]);v6[v3]=v4.catch(v5=>{v5.path="[\\"Children\\"]"+'["'+v3+'"]'+v5.path;throw v5})}catch(v5){v5.path="[\\"Children\\"]"+'["'+v3+'"]'+v5.path;throw v5}}let v7=Promise.all(v6);return Promise.all([v0,v7,]).then(([v0,v7,])=>{return {"id":v0,"children":v7,}})}`,
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
    `i=>{let v0;v0=e[0](i);return v0}
Node: i=>{typeof i==="object"&&i||e[3](i);let v0=i["Id"],v1=i["Children"];typeof v0==="string"||e[0](v0);Array.isArray(v1)||e[2](v1);let v5=new Array(v1.length);for(let v2=0;v2<v1.length;++v2){try{let v3;v3=e[1]["unknown->Node--0"](v1[v2]);v5[v2]=v3}catch(v4){v4.path="[\\"Children\\"]"+'["'+v2+'"]'+v4.path;throw v4}}return {"id":v0,"children":v5,}}`,
  )
})

test("Doesn't mutate a shared primitive schema passed as the recursive body", t => {
  let _ = S.recursive("R", _ => S.string)

  t->Assert.deepEqual(S.string->S.toExpression, "string")
  t->U.assertThrowsMessage(
    () => true->Obj.magic->S.parseOrThrow(~to=S.dict(S.string)),
    `Expected { [key: string]: string; }, received true`,
  )
})
