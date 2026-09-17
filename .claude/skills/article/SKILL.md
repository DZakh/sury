---
name: article
description: Write and translate DZakh's dev.to and Habr articles about Sury. Use for anything under docs/articles - drafting, editing a draft the author pasted in, fact-checking claims about other libraries, the Russian translation, or the Habr feed preview text.
---

# Articles

`docs/articles/<slug>.md` is the dev.to post, `<slug>.ru.md` the Habr one. Keep `published: false` until the author says otherwise. Translate the frontmatter `title` and `description`; leave `tags` and `published`.

The author usually writes or rewrites the prose himself and hands it over. When he does, **his wording is the input, not a first draft**. Fix spelling, grammar and facts. Do not improve his phrasing.

## Verify everything before it ships

Every factual claim gets executed. Behaviors, error messages, signatures, version numbers, what a competitor's helper does, what someone's docs actually say. Install the library in the scratchpad, run it, paste the real output.

This is the whole value of the job. Things it has caught:

- Zod shipped `z.validate` after a table said it had no guard at all.
- io-ts, Effect and Superstruct check the **Output** where Valibot, ArkType and TypeBox check the Input. A draft that assumed they agreed was wrong in the opposite direction.
- tRPC's docs say Zod `.describe()` **is** preserved, where the draft said descriptions were lost.
- The OpenAI SDK helper's real import path and the `draft-07` target it passes.
- A compliance number that was real but not comparable to the competitor it was compared against.

A claim about someone else's library is the one that gets the article dunked on. When a number comes from two different suites, say so or drop the comparison.

Scratch installs go in the scratchpad, never the repo. If the branch predates the release under discussion, build it in a worktree:

```bash
git worktree add <scratch>/main origin/main
cd <scratch>/main && pnpm install --frozen-lockfile && pnpm --filter=sury build:entry
```

Report what you could not verify. Say it in the reply and in the commit message, so a stale claim stays traceable.

## Voice

- First person about **decisions**: "I didn't want to give a default that throws", not "a default that throws is bad".
- Casual, direct, slightly non-native. Do not smooth it into neutral prose.
- Asides mid-paragraph: "And why should it?", "surprise,", "I know, I know".
- Rhetorical objections as blockquotes, then ignore them.
- Emoji at sentence end, sparingly: 🫡 😁 🙏 🤝 😱 👀 🧬 🙂 😄
- Publish the rows where Sury loses. Name competitors' real strengths. Undercut the headline number in the last section.
- Never tag the maintainer of anything you criticise.

**Delete on sight, these are the tells that a paragraph was not typed by a person:** three-beat sentences ("No error. No warning. Just X."), polished parallel construction, "it's time to", "Here's why", one line per beat, a closing sentence that ties a bow.

`CLAUDE.md`'s prose rules apply to `docs/` too. **No em dash**, in either language. An example over a paragraph.

## Structure

- Never restate in prose what a code block already shows. The punch goes in a comment inside the block.
- A table beats a paragraph whenever the point is "they disagree".
- Motivation before mechanics.
- Vary the closing heading between articles. `## Shipping!` is taken.
- Standard closer: the Sury link, then follow me on X, `it'll make my day 🙏`.

## Russian

Write what a Russian developer would write, not word for word. Same structure, line for line.

Keep untranslated: API names, code, identifiers, links, numbers, emoji, English slogans (`Encode, don't stringify`, `Parse, don't validate`, `pit of success`), spec terms (Input, Output), library error messages, GitHub issue titles. In a table, the library and helper columns stay byte-identical; only prose cells translate.

Decimals keep the dot (`93.4`), matching the tables and the English.

Settled words: инстанцирования типов, бейзлайн, тайп-тесты, юнион, таргет, тришейкаемый, доки, схема.

After the parity check passes, read every line again for declensions, verb government and participle agreement. The checker cannot see those.

## Parity check

```bash
python3 .claude/skills/article/parity.py docs/articles/<slug>.md
```

Structural checks fail the run; advisory ones are printed to skim. The two 2026 articles predate the no-em-dash rule and still fail it, and `json-stringify-lies-to-you` has four code blocks that drifted before this script existed. New work passes clean.

## Habr feed preview

Asked for "текст для предпросмотра", answer with a fenced block and the character count. Limit 100-3000, aim for ~1300. The feed shows about a line and a half, so the hook goes in the first sentence. Do not reuse the article's opening verbatim unless the opening *is* the hook.

## Covers

780x440, rendered from HTML:

```bash
/opt/pw-browsers/chromium-1194/chrome-linux/chrome --headless --no-sandbox \
  --disable-gpu --hide-scrollbars --window-size=780,440 \
  --screenshot=docs/articles/cover-<slug>.png cover.html
```

## Git

Branch `claude/sury-devto-article-<id>`. Commit and push after every round: the container gets reclaimed mid-session, and the pushed branch is the only thing that survives. The commit message records what was verified and what was corrected.
