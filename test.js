(i) => {
  if (!Array.isArray(i) || i.length !== 2) {
    e[3](i);
  }
  let v1 = new Array(Math.max(i[0].length, i[1].length));
  if (typeof i !== "boolean") {
    e[0](i);
  }
  for (let v0 = 0; v0 < v1.length; ++v0) {
    let v3 = i[0][v0],
      v4 = i[1][v0];
    try {
      if (!(typeof v3 === "string" || v3 === void 0)) {
        e[1](v3);
      }
      if (typeof v4 !== "boolean") {
        e[2](v4);
      }
      v1[v0] = { foo: v3, bar: v4 };
    } catch (v2) {
      if (v2 && v2.s === s) {
        v2.path = "" + "[\"'+v0+'\"]" + v2.path;
      }
      throw v2;
    }
  }
  return v1;
};
