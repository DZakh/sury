// `S.url` — a URI string on the JSON side, a `URL` on ours.
import {
  initSchema,
  instanceTag,
  type Internal,
  stringTag,
  tagFlags,
  type Val
} from "../base";
import {
  B_embed,
  B_next,
  B_pushCheck,
  B_refine,
  B_unsupportedDecode,
  failInvalidType
} from "../builder";
import {
 instanceDecoder,
 parse
} from "../parse";
import {
 stringDecoderFn
} from "../primitives";

// `new URL(…)` throws where `new Date(…)` merely yields an Invalid Date, so
// the conversion goes through a helper — a thrown TypeError would escape as
// itself rather than as a Sury error, and `URL.canParse` would parse twice.
// Falling out of the catch with no value makes the failure falsy, which is
// the whole check: a constructed URL is always truthy.
// Module scope rather than inside `init`, because the decoder that reads it is
// now built before the schema exists.
const urlFromString = (value: string) => {
  try {
    return new URL(value);
  } catch {}
};

// The `urlToUri` result, described once — same reasoning as date.ts's
// `dateTimeString`: it becomes the enclosing object's property schema and is
// reached later as another operation's target, so it needs a real decoder and a
// stable identity for the seq-keyed operation cache.
const uriString: Internal = /* @__PURE__ */ initSchema(stringTag, stringDecoderFn, (s) => {
  s.format = "uri";
  // `urlToUri` percent-encodes everything outside RFC 3986's ASCII repertoire.
  s.escapeFree = true;
});

// The decoder names `url` rather than the `init` callback's `s`: it is built
// before the schema exists, and only ever runs after.
export const url: Internal = /* @__PURE__ */ initSchema(
  instanceTag,
  (input: Val): Val => {
    const inputTagFlag = tagFlags[input.s.type]!;
    if ((inputTagFlag & 2)) {
      const constructed = B_next(
        input,
        `${B_embed(input, urlFromString)}(${input.i})`,
        url,
      );
      // The two halves of this check read different vars on purpose: the cond
      // tests the constructed URL, the failure reports the string that failed
      // to construct — `received "cifjhdsfhsd"`, not `received undefined`.
      // B_emitChecks hands one var to both, and it is `prev`'s, so the cond
      // has to close over this var name instead of taking the one it is given.
      // Materializing here is what fixes that name before the cond is built.
      const constructedVar = constructed.v();
      B_pushCheck(constructed, {
        c: () => constructedVar,
        f: failInvalidType,
      });
      return B_refine(constructed, input.e);
    } else if ((inputTagFlag & 1)) {
      return instanceDecoder(input);
    } else if ((inputTagFlag & 8192) && input.s.class === url.class) {
      return input;
    } else {
      return B_unsupportedDecode(input, input.s, input.e);
    }
  },
  (s) => {
    // WHATWG serialization is not a subset of RFC 3986, so `.href` alone would
    // make the emitted `format: "uri"` false: the path/query/fragment
    // percent-encode sets all leave `| ^ [ ]` alone, none of them covers `%`, and
    // a fragment may hold further `#`. Encoding is per component because the same
    // character is legal in one and not another — brackets delimit an IPv6 host,
    // the first `#` opens the fragment.
    //
    // RFC 3986: pchar = unreserved / pct-encoded / sub-delims / ":" / "@", with
    // "/" and "?" also free in a query or fragment. One set covers every
    // component: a path slice can never hold `?` or `#` (they open the query and
    // fragment), and WHATWG's forbidden-host and userinfo rules keep `: @ / ?`
    // out of a host, username and password — so the wider set never over-permits
    // where a narrower one would apply. `%` stays in it so a valid triplet
    // survives; a `%` starting no triplet is caught by the lookahead ahead of it.
    const encodeRe = /%(?![0-9A-Fa-f]{2})|[^A-Za-z0-9\-._~!$&'()*+,;=:@/?%]/g;
    const dirtyRe = /[^A-Za-z0-9\-._~!$&'()*+,;=:@/?#%]|%(?![0-9A-Fa-f]{2})|#[^#]*#/;
    const urlToUri = (url: URL): string => {
      const href = url.href;
      if (!dirtyRe.test(href)) {
        return href;
      }
      const protocol = url.protocol;
      let out = protocol;
      let rest = href.slice(protocol.length);
      if (rest.charCodeAt(0) === 47 && rest.charCodeAt(1) === 47) {
        out += "//";
        rest = rest.slice(2);
        let end = 0;
        while (end < rest.length) {
          const c = rest.charCodeAt(end);
          if (c === 47 || c === 63 || c === 35) {
            break;
          }
          end++;
        }
        rest = rest.slice(end);
        if (url.username || url.password) {
          out += url.username.replace(encodeRe, encodeURIComponent);
          if (url.password) {
            out += ":" + url.password.replace(encodeRe, encodeURIComponent);
          }
          out += "@";
        }
        const hostname = url.hostname;
        // An IP-literal is bracket-delimited and already RFC-valid; anything else
        // is a reg-name.
        out +=
          hostname.charCodeAt(0) === 91
            ? hostname
            : hostname.replace(encodeRe, encodeURIComponent);
        if (url.port) {
          out += ":" + url.port;
        }
      }
      // Sliced off `href` rather than read from url.search/url.hash, which drop a
      // trailing `?` or `#` that the serialization kept.
      let q = rest.indexOf("?");
      const f = rest.indexOf("#");
      if (f !== -1 && q > f) {
        q = -1;
      }
      out += rest
        .slice(0, q !== -1 ? q : f !== -1 ? f : rest.length)
        .replace(encodeRe, encodeURIComponent);
      if (q !== -1) {
        out +=
          "?" +
          rest
            .slice(q + 1, f !== -1 ? f : rest.length)
            .replace(encodeRe, encodeURIComponent);
      }
      if (f !== -1) {
        out += "#" + rest.slice(f + 1).replace(encodeRe, encodeURIComponent);
      }
      return out;
    };
    s.class = URL;
    s.encoder = (input, target) => {
      const toTagFlag = tagFlags[target.type]!;
      if ((toTagFlag & 2)) {
        // B_refine, not the bare B_next: a check emits against its val's *prev*
        // var, so the target's reversed refiner would test the `URL` this
        // converts from rather than the URI it produces — `new URL("…/a|b")`
        // failing `Expected uri` even though urlToUri hands back the escaped
        // form that satisfies it. The wrap makes the converted value the prev,
        // which is what materializes it into the var the check reads.
        return parse(
          B_refine(
            B_next(input, `${B_embed(input, urlToUri)}(${input.i})`, uriString, target)
          )
        );
      } else {
        return input;
      }
    };
  },
);
