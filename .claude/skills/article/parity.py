#!/usr/bin/env python3
"""Compare an article with its Russian translation.

    python3 parity.py docs/articles/<slug>.md

Structural checks fail the run. Everything under ADVISORY is printed for a
human to read and never fails, because only a person can tell a deliberate
rewording from a mistake.

Every check here exists because something once slipped past a read-through.
"""
import re
import sys
from pathlib import Path


def lines(text):
    return text.split("\n")


def positions(text, pred):
    return [i for i, line in enumerate(lines(text)) if pred(line)]


def code_blocks(text):
    out, cur, inside = [], [], False
    for line in lines(text):
        if line.startswith("```"):
            if inside:
                out.append("\n".join(cur))
                cur = []
            inside = not inside
        elif inside:
            cur.append(line)
    return out


def strip_comments(block):
    return "\n".join(
        l for l in block.split("\n") if not l.strip().startswith(("//", "#"))
    )


def body(text):
    parts = text.split("---", 2)
    return parts[2] if len(parts) > 2 else text


def tokens(cell):
    """Code spans and link targets inside one table cell. Prose may be
    translated; these may not."""
    return re.findall(r"`[^`]+`", cell) + re.findall(r"\]\(([^)]+)\)", cell)


def prose_paragraphs(text):
    out, cur, inside = [], [], False
    for line in lines(body(text)):
        if line.startswith("```"):
            inside = not inside
            continue
        if inside or line.startswith(("#", "-", "|", ">")):
            if cur:
                out.append(" ".join(cur))
                cur = []
            continue
        if not line.strip():
            if cur:
                out.append(" ".join(cur))
                cur = []
        else:
            cur.append(line.strip())
    if cur:
        out.append(" ".join(cur))
    return out


def main():
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    en_path = Path(sys.argv[1])
    ru_path = en_path.with_suffix(".ru.md")
    if not ru_path.exists():
        sys.exit(f"no translation at {ru_path}")
    en = en_path.read_text(encoding="utf-8")
    ru = ru_path.read_text(encoding="utf-8")

    fails = []

    def check(label, ok, detail=""):
        print(f"{'ok  ' if ok else 'FAIL'}  {label}{'  ' + detail if detail else ''}")
        if not ok:
            fails.append(label)

    def note(label, detail):
        print(f"      {label}  {detail}")

    check("line count", len(lines(en)) == len(lines(ru)),
          f"{len(lines(en))} / {len(lines(ru))}")

    for label, pred in (
        ("heading positions", lambda l: l.startswith("#")),
        ("fence positions", lambda l: l.startswith("```")),
        ("table row positions", lambda l: l.startswith("|")),
        ("list item positions", lambda l: l.startswith("- ")),
        ("blockquote positions", lambda l: l.startswith(">")),
    ):
        check(label, positions(en, pred) == positions(ru, pred),
              str(len(positions(en, pred))))

    en_langs = [l for l in lines(en) if l.startswith("```") and len(l) > 3]
    ru_langs = [l for l in lines(ru) if l.startswith("```") and len(l) > 3]
    check("fence languages", en_langs == ru_langs, " ".join(en_langs))

    be, br = code_blocks(en), code_blocks(ru)
    differing = [i for i, (a, b) in enumerate(zip(be, br))
                 if strip_comments(a) != strip_comments(b)]
    check("code blocks (comments stripped)",
          len(be) == len(br) and not differing,
          f"{len(be)} blocks" + (f", differ at {differing}" if differing else ""))
    for i in differing:
        note("block", f"#{i} - translate the comments, never the code")

    links = lambda t: sorted(re.findall(r"\]\((https?://[^)]+)\)", t))
    check("links", links(en) == links(ru), str(len(links(en))))
    for extra in sorted(set(links(en)) ^ set(links(ru))):
        note("link on one side only", extra)

    rows_en = [l for l in lines(en) if l.startswith("|")]
    rows_ru = [l for l in lines(ru) if l.startswith("|")]
    bad = []
    for a, b in zip(rows_en, rows_ru):
        ca = [c.strip() for c in a.split("|")]
        cb = [c.strip() for c in b.split("|")]
        for x, y in zip(ca, cb):
            if tokens(x) != tokens(y):
                bad.append(f"{x}  !=  {y}")
    check("table code and links", not bad)
    for row in bad:
        note("cell", row)

    emoji = lambda t: re.findall(r"[\U0001F300-\U0001FAFF✅❌]", t)
    check("emoji", emoji(en) == emoji(ru), "".join(emoji(en)))

    # CLAUDE.md prose rule. The two articles written before the rule fail here.
    check("no em dash", "—" not in en and "—" not in ru,
          f"en {en.count(chr(8212))}, ru {ru.count(chr(8212))}")

    print("\nADVISORY (never fails, read it anyway)")

    count = lambda pat, t: len(re.findall(pat, body(t)))
    for label, pat in (("inline code spans", r"`[^`\n]+`"),
                       ("bold spans", r"\*\*[^*]+\*\*")):
        a, b = count(pat, en), count(pat, ru)
        note(label, f"{a} / {b}" + ("" if a == b else "   <- differs"))

    pe, pr = prose_paragraphs(en), prose_paragraphs(ru)
    note("prose paragraphs", f"{len(pe)} / {len(pr)}")
    if len(pe) == len(pr):
        sent = lambda p: len([s for s in re.split(r"(?<=[.!?])\s+", p) if s.strip()])
        drift = [i for i, (a, b) in enumerate(zip(pe, pr)) if sent(a) != sent(b)]
        note("sentence-count drift", str(drift) if drift else "none")
        for i in drift[:5]:
            note("  en", pe[i][:100])
            note("  ru", pr[i][:100])

    # Latin left in the Russian prose. Proper nouns and spec terms belong
    # here, so this is a list to skim, not a verdict.
    text = re.sub(r"```.*?```", "", body(ru), flags=re.S)
    text = re.sub(r"\[([^\]]*)\]\([^)]*\)", r"\1", text)
    text = re.sub(r"`[^`]*`", "", text)
    words = sorted({w.rstrip(".,!?:;-") for w in
                    re.findall(r"[A-Za-z][A-Za-z0-9@/._-]{2,}", text)})
    note("latin in ru prose", " ".join(words) or "none")

    print()
    if fails:
        print(f"{len(fails)} failed: {', '.join(fails)}")
        return 1
    print("parity ok")
    return 0


if __name__ == "__main__":
    sys.exit(main())
