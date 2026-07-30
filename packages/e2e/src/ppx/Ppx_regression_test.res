open Vitest

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
      ~op=#Encode,
      `i=>{for(;;){if(typeof i==="object"&&i&&!Array.isArray(i)&&i["TAG"]==="A"){let v0=i["_0"];typeof v0==="object"&&v0&&!Array.isArray(v0)||e[2](v0);let v1=v0["payload"];typeof v1==="object"&&v1&&!Array.isArray(v1)||e[1](v1);let v2=v1["a"];(typeof v2==="string"||v2===void 0)||e[0](v2);i={"payload":{"a":v2,},};break}if(typeof i==="object"&&i&&!Array.isArray(i)&&i["TAG"]==="B"){let v3=i["_0"];typeof v3==="object"&&v3&&!Array.isArray(v3)||e[5](v3);let v4=v3["payload"];typeof v4==="object"&&v4&&!Array.isArray(v4)||e[4](v4);let v5=v4["b"];(typeof v5==="number"&&!Number.isNaN(v5)&&v5<=2147483647&&v5>=-2147483648&&v5%1===0||v5===void 0)||e[3](v5);i={"payload":{"b":v5,},};break}e[6](i)}return i}`,
    )

    let x = {
      B.payload: {
        b: 42,
      },
    }
    t->Assert.deepEqual(B(x)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`{"payload":{"b":42}}`))
    let x = {
      A.payload: {
        a: "foo",
      },
    }
    t->Assert.deepEqual(A(x)->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`{"payload":{"a":"foo"}}`))
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
      `i=>{typeof i==="object"&&i&&!Array.isArray(i)||e[3](i);let v0=i["test"];for(;;){if(typeof v0==="object"&&v0&&!Array.isArray(v0)&&v0["type"]==="a"){let v1=v0["x"];typeof v1==="number"&&v1<=2147483647&&v1>=-2147483648&&v1%1===0||e[0](v1);v0={"TAG":"A","_0":{"x":v1,},};break}if(typeof v0==="object"&&v0&&!Array.isArray(v0)&&v0["type"]==="b"){let v2=v0["y"];typeof v2==="string"||e[1](v2);v0={"TAG":"B","_0":{"y":v2,},};break}if(v0===void 0)break;e[2](v0)}return {"test":v0,}}`,
    )

    t->Assert.deepEqual(S.decodeOrThrow("{}", ~from=S.jsonString, ~to=schema), {test: None})
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
      `i=>{for(;;){let r;if(typeof i==="object"&&i&&!Array.isArray(i)){try{let v0=i["statusCode"];typeof v0==="object"&&v0&&!Array.isArray(v0)&&v0["kind"]==="ok"||e[0](v0);i={"TAG":"Ok","_0":void 0,};break}catch(x){(r||(r=[])).push(e[4](x))}try{let v1=i["statusCode"];typeof v1==="object"&&v1&&!Array.isArray(v1)&&v1["kind"]==="serviceError"||e[3](v1);let v2=v1["serviceCode"],v3=v1["text"];typeof v2==="string"||e[1](v2);typeof v3==="string"||e[2](v3);i={"TAG":"Error","_0":{"serviceCode":v2,"text":v3,},};break}catch(x){(r||(r=[])).push(e[4](x))}}e[5](i,...(r||[]))}return i}`,
    )

    t->Assert.deepEqual(S.decodeOrThrow(`{"statusCode": {"kind": "ok"}}`, ~from=S.jsonString, ~to=schema), Ok())
  })
}
