open Ava

module CknittelBugReport = {
  module A = {
    @schema
    type payload = {a?: string}

    @schema
    type t = {payload: payload}
  }

  module B = {
    @schema
    type payload = {b?: int}

    @schema
    type t = {payload: payload}
  }

  type value = A(A.t) | B(B.t)

  test("Union serializing of objects with optional fields", t => {
    let schema = S.union([A.schema->S.shape(m => A(m)), B.schema->S.shape(m => B(m))])

    t->U.assertCompiledCode(
      ~schema,
      ~op=#ReverseConvert,
      `i=>{if(typeof i==="object"&&i){if(i["TAG"]==="A"&&typeof i["_0"]==="object"&&i["_0"]&&typeof i["_0"]["payload"]==="object"&&i["_0"]["payload"]){let v0=i["_0"];let v1=v0["payload"];i=v0}else if(i["TAG"]==="B"&&typeof i["_0"]==="object"&&i["_0"]&&typeof i["_0"]["payload"]==="object"&&i["_0"]["payload"]){let v2=i["_0"];let v3=v2["payload"];i=v2}}return i}`,
    )

    let x = {
      B.payload: {
        b: 42,
      },
    }
    t->Assert.deepEqual(B(x)->S.reverseConvertOrThrow(schema), %raw(`{"payload":{"b":42}}`))
    let x = {
      A.payload: {
        a: "foo",
      },
    }
    t->Assert.deepEqual(A(x)->S.reverseConvertOrThrow(schema), %raw(`{"payload":{"a":"foo"}}`))
  })
}

module CknittelBugReport2 = {
  @schema
  type a = {x: int}

  @schema
  type b = {y: string}

  type test = A(a) | B(b)

  let testSchema = S.union([
    S.object(s => {
      s.tag("type", "a")
      A(s.flatten(aSchema))
    }),
    S.object(s => {
      s.tag("type", "b")
      B(s.flatten(bSchema))
    }),
  ])

  @schema
  type t = {test: option<test>}

  test("Successfully parses nested optional union", t => {
    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i!=="object"||!i){e[0](i)}let v0=i["test"];if(typeof v0==="object"&&v0){if(v0["type"]==="a"){let v1=v0["x"];if(typeof v1!=="number"||v1>2147483647||v1<-2147483648||v1%1!==0){e[1](v1)}v0={"TAG":e[2],"_0":{"x":v1,},}}else if(v0["type"]==="b"){let v2=v0["y"];if(typeof v2!=="string"){e[3](v2)}v0={"TAG":e[4],"_0":{"y":v2,},}}else{e[5](v0)}}else if(!(v0===void 0)){e[6](v0)}return {"test":v0,}}`,
    )

    t->Assert.deepEqual(S.parseJsonStringOrThrow("{}", schema), {test: None})
  })

  type responseError = {serviceCode: string, text: string}

  test("Nested literal field with catch", t => {
    let schema = S.union([
      S.object(s => {
        let _ = s.nested("statusCode").field("kind", S.literal("ok"))
        Ok()
      }),
      S.object(s => {
        let _ = s.nested("statusCode").field("kind", S.literal("serviceError"))
        Error({
          serviceCode: s.nested("statusCode").field("serviceCode", S.string),
          text: s.nested("statusCode").field("text", S.string),
        })
      }),
    ])

    t->U.assertCompiledCode(
      ~schema,
      ~op=#Parse,
      `i=>{if(typeof i==="object"&&i){if(typeof i["statusCode"]==="object"&&i["statusCode"]&&i["statusCode"]["kind"]==="ok"){let v0=i["statusCode"];i={"TAG":e[0],"_0":e[1],}}else if(typeof i["statusCode"]==="object"&&i["statusCode"]&&i["statusCode"]["kind"]==="serviceError"){let v1=i["statusCode"],v2=v1["serviceCode"],v3=v1["text"];if(typeof v2!=="string"){e[2](v2)}if(typeof v3!=="string"){e[3](v3)}i={"TAG":e[4],"_0":{"serviceCode":v2,"text":v3,},}}else{e[5](i)}}else{e[6](i)}return i}`,
    )

    t->Assert.deepEqual(S.parseJsonStringOrThrow(`{"statusCode": {"kind": "ok"}}`, schema), Ok())
  })
}
