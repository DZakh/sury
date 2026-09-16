---
title: Я профессионально проектирую библиотеки уже 5 лет
published: true
description: Как AI НЕ изменил ничего, и почему?
tags: typescript, opensource, ai, webdev
# cover_image: https://direct_url_to_image.jpg
# Use a ratio of 100:42 for best results.
# published_at: 2026-06-16 17:36 +0000
---

7 лет назад я опубликовал в npm свою первую библиотеку - [custom-border-mixin](https://www.npmjs.com/package/custom-border-mixin). Тогда я на неделю пропал, просто чтобы в своё удовольствие написать SCSS-миксин. Если вам интересно, что весёлого может быть в SCSS, просто посмотрите на хелпер:

```scss
@function param-get($parameters, $key) {
  $value: map-get($parameters, $key);
  @if $key == 'side' and $value != 'top' and $value != 'right' and $value != 'bottom' and $value != 'left' {
    @error 'Value #{$value} of property #{$key} must be either top, or right, or bottom, or left, or vertical, or horizontal, or all.';
  }
  @if ($key == 'size' or $key == 'length' or $key == 'gap') and (type-of($value) != number or type-of($value) == number and $value < 0) {
    @error 'Value #{$value} of property #{$key} must be non-negative size number.';
  }
  @if $key == 'color' and type-of($value) != color {
    @error 'Value #{$value} of property #{$key} must be color.';
  }
  @if $key == 'start' and $value != 'origin' and $value != 'center' and $value != 'opposite' {
    @error 'Value #{$value} of property #{$key} must be either origin, center, or opposite.';
  }
  @return $value;
}
```

Миксин никому не был нужен, и никто о нём не просил, но, кажется, он зажёг во мне какую-то страсть. Наверное, так многие и приходят в опенсорс.

Меня зовут [Дмитрий](https://x.com/dzakh_dev), и в этой статье я расскажу, как наступление агентного программирования повлияло на мой взгляд на дизайн библиотек и публичных API. И как оно на самом деле ничего не изменило в том, что делает библиотеку хорошей.

Я работал в платформенной команде, а 5 лет назад завёл свой личный опенсорс-проект [Sury](https://github.com/DZakh/sury). Уже v11, и он всё ещё жив. Сейчас я работаю в [Envio](https://envio.dev/), где придумываю и строю самый быстрый инструмент для индексации блокчейна ([HyperIndex на GitHub](https://github.com/enviodev/hyperindex)).

## Как AI изменил дизайн API библиотек?

В идеальном мире между API для человека и для AI-агента не должно быть почти никакой разницы. Просто так вышло, что раньше, проектируя библиотеки, мы часто рассчитывали на то, что пользователь прочитает доки, что-то уже знает или у него есть _контекст_. С агентами это работает не всегда, поэтому важно спроектировать библиотеку так, чтобы API само вело пользователя...

**...в тот самый pit of success** - да-да, я знаю, но это не стареет.

Кроме очевидных вещей вроде подсказывающих сообщений об ошибках, вот несколько практик, которые я применил в релизе [Sury](https://github.com/DZakh/sury) v11. Sury - это JavaScript-библиотека схем, дальше все примеры будут на ней.

## 1. Явное лучше неявного

В Sury нет `parse`. Я не хотел давать дефолт, который где-то там кинет исключение, и никто его не обработает. Поэтому есть два имени, и вам придётся выбрать:

```ts
S.parseOrThrow(userSchema, data);
// { id: "p_1" }

S.parseAsResult(userSchema, data);
// { success: true, value: { id: "p_1" } }
// { success: false, error: SuryError: Expected string, received 42 }
```

Теперь выбор зафиксирован в коде. Агент скорее возьмёт `parseAsResult`, а на ревью другой агент реально увидит, что ошибку никто не обрабатывает.

Сравните с Zod, где короткое имя досталось тому, который кидает:

```ts
userSchema.parse(data); // throws
userSchema.safeParse(data); // returns a result
```

Если убрать строчку с `safeParse`, то `parse` выглядит ровно так же безопасно, правда? Ничто в `parse` не говорит, что сейчас прилетит исключение, а безопасный вариант спрятан за именем, о котором надо знать заранее.

Иронично, но сейчас это важно даже больше, потому что ревью делает AI. У человека хотя бы могло возникнуть нехорошее предчувствие насчёт `parse`.

### Та же история, только хуже

Теперь посмотрите на `is`/`validate`. Что-то такое есть в каждой библиотеке схем, и на первый взгляд выглядит совершенно безобидно:

```ts
if (is(userSchema, data)) {
  // data ведь User, да?
}
```

Но если схема что-то трансформирует, у этого вопроса два ответа. Каждая библиотека выбирает один за вас, и, сюрприз, выбирают они по-разному:

| | Хелпер | Что проверяет |
|---|---|---|
| [Zod](https://zod.dev/) | `z.validate(schema, data)` | Input |
| [Valibot](https://valibot.dev/) | `v.is(schema, data)` | Input |
| [ArkType](https://arktype.io/) | `schema.allows(data)` | Input |
| [TypeBox](https://github.com/sinclairzx81/typebox) | `Value.Check(schema, data)` | Input |
| [io-ts](https://github.com/gcanti/io-ts) | `codec.is(data)` | Output |
| [Effect](https://effect.website/) | `Schema.is(schema)(data)` | Output |
| [Superstruct](https://github.com/ianstormtaylor/superstruct) | `is(data, struct)` | Output |
| [Yup](https://github.com/jquense/yup) | `schema.isValidSync(data)` | сначала конвертирует, проходят оба |
| [Joi](https://joi.dev/) | `schema.validate(data)` | сначала конвертирует, проходят оба |
| [Sury](https://github.com/DZakh/sury) | `S.isInput` / `S.isOutput` | тот, который вы выбрали |

Один и тот же вызов, противоположный смысл, в зависимости от того, что у вас в `package.json`. А Yup с Joi сначала конвертируют значение и поэтому просто говорят «да» обоим.

Замаскированная русская рулетка. 👀

Вот поэтому `S.is` не существует:

```ts
const priceSchema = S.string.with(S.to, S.number);

S.isInput(priceSchema, "42"); // true
S.isOutput(priceSchema, "42"); // false
```

## 2. Любой способ там, где это неважно

Агент, который не угадал порядок аргументов, тратит на это лишнюю итерацию. С людьми то же самое, только они хотя бы запомнят со второго раза. Но зачем им это? Поэтому я сделал так, чтобы работал любой порядок:

```ts
S.parseOrThrow(data, userSchema);
S.parseOrThrow(userSchema, data);
S.parseOrThrow(userSchema)(data);

S.isInput(data, userSchema);
S.isInput(userSchema, data);
S.isInput(userSchema)(data);
```

Все они дают одинаковый результат, так что любая догадка - правильная.

## 3. Заставить выбрать там, где это важно

Когда кейс можно прочитать по-разному, я не хочу выбирать дефолт за вас. Я заставляю выбрать:

```ts
S.decodeOrThrow(S.env, S.string);
// SuryError: Ambiguous "" for string. Should a blank input be rejected,
// kept, or read as absent? Choose with S.nonEmpty, S.minLength(0),
// or S.optional
```

И падает это в момент сборки декодера, а не когда придут данные, так что вы увидите это, пока пишете код. Пустая переменная окружения - это решение, и мне кажется, моя библиотека не должна принимать его за вас.

## 4. Алиасы под общеизвестное

Как бы мне ни хотелось продавить своё API, агент или человек сначала пробует синтаксис из своих знаний. Борьба с этим стоит лишней итерации и раздражения, поэтому я просто сделал алиасы:

```ts
S.union([S.literal("admin"), S.object({ role: S.literal("user") })]);
S.union([S.schema("admin"), S.schema({ role: S.schema("user") })]);
S.union(["admin", { role: "user" }]);
// все три: Schema<"admin" | { role: "user" }>
```

А если какое-то из написаний вам в своей кодовой базе не нравится, то это правило линтера, а не решение, которое я должен принять за всех:

```js
// eslint.config.js
"no-restricted-syntax": ["error", {
  selector: "CallExpression[callee.object.name='S'][callee.property.name='object']",
  message: "Use S.schema instead of S.object",
}]
```

## Возвращаясь к pit of success

Честно говоря, всё это вообще не про AI. Библиотека, которая сама ведёт пользователя, и для людей всегда была лучше. Просто агенты перестали прощать то, что мы раньше закрывали документацией.

Всё это есть в [Sury](https://github.com/DZakh/sury) v11, который уже вышел. А если хотите больше про библиотеки схем и дизайн библиотек, подписывайтесь на меня в [X](https://x.com/dzakh_dev) - это сделает мой день 🙏
