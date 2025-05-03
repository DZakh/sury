(i) => {
  if (typeof i === "object" && i) {
    if (
      typeof i["foo"] === "object" &&
      i["foo"] &&
      typeof i["foo"]["tag"] === "object" &&
      i["foo"]["tag"] &&
      i["foo"]["tag"]["NAME"] === "Null"
    ) {
      let v0 = i["foo"];
      let v1 = v0["tag"];
      let v2 = v1["VAL"];
      if (v2 === void 0) {
        v2 = null;
      }
      i = { foo: { tag: { NAME: v1["NAME"], VAL: v2 } } };
    } else if (
      typeof i["foo"] === "object" &&
      i["foo"] &&
      typeof i["foo"]["tag"] === "object" &&
      i["foo"]["tag"] &&
      i["foo"]["tag"]["NAME"] === "Option"
    ) {
      let v3 = i["foo"];
      let v4 = v3["tag"];
      i = i;
    }
  }
  return i;
};
