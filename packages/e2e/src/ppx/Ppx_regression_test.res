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
      `i=>{try{if(typeof i==="object"&&i&&!Array.isArray(i)){for(;;){if(i.TAG==="A"){let v0=i._0;let v1=v0.payload;i=v0;break}if(i.TAG==="B"){let v2=i._0;let v3=v2.payload;i=v2;break}throw e[0](i)}}else{throw e[0](i)}return i}catch(v4){e[1](v4)}}`,
    )

    let x = {
      B.payload: {
        b: 42,
      },
    }
    t->Assert.deepEqual(B(x)->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`{"payload":{"b":42}}`))
    let x = {
      A.payload: {
        a: "foo",
      },
    }
    t->Assert.deepEqual(A(x)->S.convertOrThrow(~from=schema, ~to=S.unknown), %raw(`{"payload":{"a":"foo"}}`))
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
      `i=>{try{typeof i==="object"&&i&&!Array.isArray(i)||e[3](i);let v0=i.test;for(;;){if(typeof v0==="object"&&v0&&!Array.isArray(v0)){for(;;){if(v0.type==="a"){let v1=v0.x;typeof v1==="number"&&v1<=2147483647&&v1>=-2147483648&&v1%1==0||e[0](v1);v0={TAG:"A",_0:{x:v1}};break}if(v0.type==="b"){let v2=v0.y;typeof v2==="string"||e[1](v2);v0={TAG:"B",_0:{y:v2}};break}throw e[2](v0)};break}if(v0===void 0)break;throw e[2](v0)}return {test:v0}}catch(v3){e[4](v3)}}`,
    )

    t->Assert.deepEqual(S.convertOrThrow(S.JsonString("{}"), ~from=S.jsonString, ~to=schema), {test: None})
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
      `i=>{try{let v1;for(;;){if(typeof i==="object"&&i&&!Array.isArray(i)){l2:{let v0=i.statusCode;if(!(typeof v0==="object"&&v0&&!Array.isArray(v0)&&v0.kind==="ok")){(v1||(v1=[])).push(e[0],v0,void 0);break l2};i={TAG:"Ok",_0:void 0};break}l5:{let v3=i.statusCode;if(!(typeof v3==="object"&&v3&&!Array.isArray(v3)&&v3.kind==="serviceError")){(v1||(v1=[])).push(e[3],v3,void 0);break l5};let v4=v3.serviceCode,v6=v3.text;if(!(typeof v4==="string")){(v1||(v1=[])).push(e[1],v4,void 0);break l5};if(!(typeof v6==="string")){(v1||(v1=[])).push(e[2],v6,void 0);break l5};i={TAG:"Error",_0:{serviceCode:v4,text:v6}};break}}throw e[4](i,void 0,v1)}return i}catch(v7){e[5](v7)}}`,
    )

    t->Assert.deepEqual(S.convertOrThrow(S.JsonString(`{"statusCode": {"kind": "ok"}}`), ~from=S.jsonString, ~to=schema), Ok())
  })
}
