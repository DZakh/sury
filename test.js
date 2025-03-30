(i) => {
  let v1 = [new Array(i.length), new Array(i.length), new Array(i.length)];
  for (let v0 = 0; v0 < i.length; ++v0) {
    let v3 = i[v0];
    try {
      let v4 = v3["name"];
      if (v4 === void 0) {
        v4 = null;
      }
      v1[0][v0] = v3["id"];
      v1[1][v0] = v4;
      v1[2][v0] = v3["deleted"];
    } catch (v2) {
      if (v2 && v2.s === s) {
        v2.path = "" + "[\"'+v0+'\"]" + v2.path;
      }
      throw v2;
    }
  }
  return v1;
};
