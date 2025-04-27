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
      `i=>{if(typeof i==="object"&&i){if(i["TAG"]==="A"){let v0=i["TAG"],v1=i["_0"];if(v0!=="A"){e[0](v0)}let v2=v1["payload"];i=v1}else if(i["TAG"]==="B"){let v4=i["TAG"],v5=i["_0"];if(v4!=="B"){e[1](v4)}let v6=v5["payload"];i=v5}}return i}`,
    )

    let x = {
      B.payload: {
        b: 42,
      },
    }
    t->Assert.deepEqual(B(x)->S.reverseConvertOrThrow(schema), %raw(`{"payload":{"b":42}}`), ())
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
      `i=>{if(typeof i!=="object"||!i){e[6](i)}let v0=i["test"];if(typeof v0==="object"&&v0){if(v0["type"]==="a"){let v1=v0["x"];if(typeof v1!=="number"||v1>2147483647||v1<-2147483648||v1%1!==0){e[0](v1)}v0={"TAG":e[1],"_0":{"x":v1,},}}else if(v0["type"]==="b"){let v2=v0["y"];if(typeof v2!=="string"){e[2](v2)}v0={"TAG":e[3],"_0":{"y":v2,},}}else{e[4](v0)}}else if(!(v0===void 0)){e[5](v0)}return {"test":v0,}}`,
    )

    t->Assert.deepEqual(S.parseJsonStringOrThrow("{}", schema), {test: None}, ())
  })

  type responseError = {serviceCode: string, text: string}

  test("Nested literal field with catch", t => {
    let schema = S.union([
      S.object(s => {
        let _ = s.nested("statusCode").field("kind", S.literal("ok"))
        let _ = s.nested("statusCode").field("text", S.literal("")->S.catch(_ => ""))
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
      `i=>{if(typeof i==="object"&&i){try{let v0=i["statusCode"];if(typeof v0!=="object"||!v0||v0["kind"]!=="ok"){e[0](v0)}let v1=v0["text"];try{if(v1!==""){e[2](v1)}}catch(v2){if(v2&&v2.s===s){v1=e[1](v1,v2)}else{throw v2}}i={"TAG":e[3],"_0":e[4],}}catch(e0){try{let v3=i["statusCode"];if(typeof v3!=="object"||!v3||v3["kind"]!=="serviceError"){e[5](v3)}let v4=v3["serviceCode"],v5=v3["text"];if(typeof v4!=="string"){e[6](v4)}if(typeof v5!=="string"){e[7](v5)}i={"TAG":e[8],"_0":{"serviceCode":v4,"text":v5,},}}catch(e1){e[9](i,e0,e1)}}}else{e[10](i)}return i}`,
    )

    t->Assert.deepEqual(
      S.parseJsonStringOrThrow(`{"statusCode": {"kind": "ok"}}`, schema),
      Ok(),
      (),
    )
  })
}
