# Contributing

When it comes to open source, there are different ways you can contribute, all of which are valuable. Here's few guidelines that should help you as you prepare your contribution.

## Initial steps

Before you start working on a contribution, create an issue describing what you want to build. It's possible someone else is already working on something similar, or perhaps there is a reason that feature isn't implemented. The maintainers will point you in the right direction.

## Development

The following steps will get you setup to contribute changes to this repo:

1. Fork this repo.
2. Clone your forked repo: `git clone git@github.com:{your_username}/rescript-schema.git`
3. Install [pnpm](https://pnpm.io/) if not available `npm i -g pnpm@8.14.3`
4. Run `pnpm i` to install dependencies.
5. Start playing with the code!

## PPX

### With Dune

Make sure running the below commands in `packages/rescript-schema-ppx/src`.

1. Create a sandbox with opam

```
opam switch create rescript-schema-ppx 4.12.1
```

2. Install dependencies

```
opam install . --deps-only
```

3. Build

```
dune build --watch
```

4. Test

Make sure running tests

```
(run compiler for lib)
npm run res
(run compiler for tests)
npm run test:res
(run tests in watch mode)
npm run test -- --watch
```

## Make comparison

https://bundlejs.com/

`rescript-schema`

```ts
import * as S from "rescript-schema@9.0.0-rc.2";

// Create login schema with email and password
const loginSchema = S.schema({
  email: S.email(S.string),
  password: S.stringMinLength(S.string, 8),
});

// Infer output TypeScript type of login schema
type LoginData = S.Output<typeof loginSchema>; // { email: string; password: string }

// Throws the S.Error(`Failed parsing at ["email"]. Reason: Invalid email address`)
S.parseOrThrow({ email: "", password: "" }, loginSchema);

// Returns data as { email: string; password: string }
S.parseOrThrow(
  {
    email: "jane@example.com",
    password: "12345678",
  },
  loginSchema
);
```

valibot

```ts
import * as v from "valibot@0.42.1"; // 1.21 kB

// Create login schema with email and password
const LoginSchema = v.object({
  email: v.pipe(v.string(), v.email()),
  password: v.pipe(v.string(), v.minLength(8)),
});

// Infer output TypeScript type of login schema
type LoginData = v.InferOutput<typeof LoginSchema>; // { email: string; password: string }

// Throws error for `email` and `password`
v.parse(LoginSchema, { email: "", password: "" });

// Returns data as { email: string; password: string }
v.parse(LoginSchema, { email: "jane@example.com", password: "12345678" });
```

zod

```ts
import * as z from "zod";

// Create login schema with email and password
const LoginSchema = z.object({
  email: z.string().email(),
  password: z.string().min(8),
});

// Throws error for `email` and `password`
LoginSchema.parse({ email: "", password: "" });

// Returns data as { email: string; password: string }
LoginSchema.parse({ email: "jane@example.com", password: "12345678" });
```

ArkType

```ts
import { type } from "arktype@2.1.0";

const loginSchema = type({
  email: "string.email",
  password: "string > 7",
});

loginSchema({ email: "", password: "" });

loginSchema({ email: "jane@example.com", password: "12345678" });
```

https://bundlejs.com/?q=arktype%402.0.4&treeshake=%5B*%5D&text=%22const+loginSchema+%3D+type%28%7B%5Cn++email%3A+%5C%22string.email%5C%22%2C%5Cn++password%3A+%5C%22string+%3E+7%5C%22%2C%5Cn%7D%29%3B%5Cn%5CnloginSchema%28%7B+email%3A+%5C%22%5C%22%2C+password%3A+%5C%22%5C%22+%7D%29%3B%5Cn%5CnloginSchema%28%7B+email%3A+%5C%22jane%40example.com%5C%22%2C+password%3A+%5C%2212345678%5C%22+%7D%29%3B%22

## License

By contributing your code to the rescript-schema GitHub repository, you agree to license your contribution under the MIT license.
