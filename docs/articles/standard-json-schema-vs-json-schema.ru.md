---
title: Standard JSON Schema vs JSON Schema
published: true
description: JSON Schema - стандарт для описания JSON. Standard JSON Schema - стандарт для...
tags: typescript, json, opensource, webdev
# cover_image: https://direct_url_to_image.jpg
# Use a ratio of 100:42 for best results.
# published_at: 2026-06-16 17:36 +0000
---

Чёрт, как же я люблю стандарты! Живёшь и радуешься: наконец-то почти всё можно воткнуть в USB-C... Ой, статья же не про USB-C, а про то, что в 2026 году JSON Schema наконец обзавелась «Standard»!

Ладно, это шутка... JSON Schema и сама по себе стандарт, причём давно, и даже не один: первое предложение в 2007-м, draft-04, draft-07, draft-2020-12, openapi-3.0, и они не заканчиваются.

Описывать JSON с помощью JSON - что может быть лучше? Вот что такое JSON Schema:

```json
{
  "type": "object",
  "properties": {
    "id": { "type": "string" },
    "price": { "type": "number" }
  },
  "required": ["id", "price"]
}
```

> Эй, это все и так знают! Расскажи лучше про Standard JSON Schema!!!

.
.
> ... Игнорирую

Даже если вы не знаете JSON Schema и никогда её не писали, она занимает немалое место в вашей жизни. Уверен, хоть чем-то из этого вы пользовались:

- OpenAPI
- OpenAI
- OpenTelemetry
- OpenRPC
- Ope...

Стоп, тут пахнет заговором приставки Open. Так что давайте остановимся, и я объясню, как все эти слова связаны с JSON Schema.

[OpenAPI](https://www.openapis.org/) - стандарт, который описывает HTTP API через JSON Schema. [OpenAI](https://openai.com/) попал сюда не потому, что у меня сегодня игривое настроение, - хотя и поэтому тоже. Он здесь потому, что современные AI-модели (не только у OpenAI) поддерживают JSON Schema, чтобы гарантировать структуру ответа. [OpenTelemetry](https://opentelemetry.io/docs/collector/configuration/) - хороший пример того, как JSON Schema используют для конфигов. Почти вся валидация и подсказки при наведении в конфигах IDE работают на JSON Schema. [OpenRPC](https://www.open-rpc.org/) - не знаю, я и сам только что узнал, что такое существует.

Если продолжать, JSON Schema встречается то тут, то там. Особенно модно это было лет 7 назад, когда [Fastify](https://fastify.dev/) был ещё молодым и блестящим, - хотел я сказать, но посмотрите на их обновлённый сайт!

А ещё формы...

> Стоп, но в здравом уме никто не делает формы на JSON Schema!

Ну, для декларативного UI это может быть интересно, но в плане валидации все массово ушли в свои библиотеки схем:

```ts
import Joi from "joi";

const schema = Joi.object({
  id: Joi.string().required(),
  price: Joi.number().required(),
});
```

Идея зашла, и теперь у нас сотни библиотек схем. У каждой свой API, своя логика, свои типы и своё внутреннее представление. Представление, которое никогда не совместимо ни с JSON Schema, ни с кучей библиотек, уже работавших с JSON Schema до этого.

## Ну вот, мы снова в деле!

Тогда-то [@colinhacks](https://x.com/colinhacks) (Zod), [@fabianhiller](https://x.com/fabianHiller) (Valibot) и [@ssalbdivad](https://x.com/ssalbdivad) (ArkType) собрались вместе и сделали [Standard Schema](https://standardschema.dev/). А через несколько месяцев добавили расширение - [Standard JSON Schema](https://standardschema.dev/json-schema).

Теперь сотни библиотек схем, каждая из которых раньше изобретала свой велосипед, получили стандарт: как отдавать JSON Schema, чтобы другие библиотеки забирали её одинаково.

Другими словами, [Standard JSON Schema](https://standardschema.dev/json-schema) - это стандартный способ для библиотек схем вроде Zod отдавать [JSON Schema](https://json-schema.org/), чтобы другие библиотеки, которые знают стандарт, могли ей пользоваться, не зная, что пришла она из Zod.

**Стандарт для передачи JSON Schema. Буквально.**

Звучит скромно, но на самом деле это очень круто, и мне правда хочется, чтобы этим пользовались шире - ради будущего экосистемы JavaScript.

Почему я считаю это важным? Знаете, как [tRPC](https://trpc.io/) сейчас поддерживает OpenAPI? Он выводит типы входа и выхода во время компиляции проекта и по ним строит JSON Schema. Отличное решение, которое работает со всеми существующими библиотеками схем, но форматы и дополнительные метаданные по дороге теряются. И каждой библиотеке, которая сегодня принимает хоть какие-то схемы, приходится изобретать велосипед заново.

Теперь это решено, и я ставлю на то, что tRPC совсем скоро начнёт использовать Standard JSON Schema. Хотя бы чтобы дополнять ту JSON Schema, которую они вывели из типов.

Честно говоря, распространение пока небольшое. Но из хорошего: крупные проекты вроде OpenAI SDK недавно добавили поддержку Standard JSON Schema. И моё мнение такое: если вы делаете библиотеку, которой нужно знать типы в рантайме, обязательно загляните в доки [Standard JSON Schema](https://standardschema.dev/json-schema).

## Как этим пользоваться?

В некоторых библиотеках это есть из коробки:

```ts
import type { StandardJSONSchemaV1 } from "@standard-schema/spec";
import * as z from "zod";

z.string() satisfies StandardJSONSchemaV1; // ✅
```

Так что схему можно передавать куда угодно и работать с ней, не завязываясь на конкретную библиотеку:

```ts
// Функция принимает любой совместимый `StandardJSONSchemaV1`
// и превращает его в JSON Schema.
export function acceptSchema(schema: StandardJSONSchemaV1) {
  // делаем что-нибудь, например
  return schema["~standard"].jsonSchema.input({
    target: "draft-2020-12",
  });
}

acceptSchema(z.string());
```

Для некоторых библиотек, например [Valibot](https://valibot.dev/), в приоритете размер бандла. Поэтому вместо того, чтобы добавлять каждому пользователю лишние 1-4 КБ, стандарт вынесен в отдельную функцию или даже в отдельный пакет:

```ts
import * as v from "valibot";
import { toStandardJsonSchema } from "@valibot/to-json-schema";

toStandardJsonSchema(v.string()) satisfies StandardJSONSchemaV1; // ✅
```

Ещё интересно, как с этим справляется [Sury](https://github.com/DZakh/sury). По умолчанию Standard JSON Schema выключена у всех схем. На бандл это не влияет никак. А есть тришейкаемый тумблер `S.enableStandardJSONSchema()`, который автоматически включает Standard JSON Schema для всех схем - и существующих, и будущих.

```ts
import * as S from "sury";

S.enableStandardJSONSchema();

S.string satisfies StandardJSONSchemaV1; // ✅
```

## Из JSON Schema в Standard JSON Schema

По умолчанию это две разные вещи, и если API библиотеки принимает Standard JSON Schema, это не значит, что он примет саму JSON Schema.

Нужно это нечасто, но, думаю, кому-то из вас пригодится. И решение есть приятное. Например, через [Sury](https://github.com/DZakh/sury), которую я упоминал выше, можно сделать конвертацию, и что особенно круто - вы получите ещё и типы:

```ts
import * as S from "sury";

const schema = S.fromJSONSchemaOrThrow({
  type: "object",
  properties: { id: { type: "string" }, role: { enum: ["admin", "user"] } },
  required: ["id"],
});
//? S.Schema<{ id: string; role?: "admin" | "user" | undefined }>

S.enableStandardJSONSchema();

schema satisfies StandardJSONSchemaV1; // ✅
```

Зачем это может понадобиться? Вернуться к истокам и взять зрелый DSL самой JSON Schema? Или, скажем, сконвертировать draft-07 в openapi-3.0 - это тоже работает, потому что Standard JSON Schema поддерживает разные таргеты. На день написания статьи у [Sury](https://github.com/DZakh/sury) 93.4% соответствия draft-07. Почти столько же, сколько у [TypeBox](https://github.com/sinclairzx81/typebox), только TypeBox не совместим со Standard JSON Schema и не умеет конвертировать в другой таргет JSON Schema.

## Выкатываем!

Надеюсь, вам понравилось знакомство со [Standard JSON Schema](https://standardschema.dev/json-schema). У проекта огромный потенциал, и я надеюсь, что со временем он объединит экосистему JavaScript. Распространение пока небольшое, но я уверен, что это изменится.

Ещё раз спасибо [@colinhacks](https://x.com/colinhacks) (Zod), [@fabianhiller](https://x.com/fabianHiller) (Valibot), [@ssalbdivad](https://x.com/ssalbdivad) (ArkType) и сообществу за то, что стандарт появился на свет.

А если хотите больше про библиотеки схем и всё вокруг них, подписывайтесь на меня в [X](https://x.com/dzakh_dev) - это сделает мой день 🙏
