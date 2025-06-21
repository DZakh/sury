(i) => {
  let r0 = (i) => {
    if (typeof i === "object" && i) {
      if (i["TAG"] === "A") {
        let v0 = i["_0"],
          v4 = new Array(v0.length);
        for (let v1 = 0; v1 < v0.length; ++v1) {
          let v3;
          try {
            v3 = r0(v0[v1]);
          } catch (v2) {
            if (v2 && v2.s === s) {
              v2.path = '["_0"]' + '["' + v1 + '"]' + v2.path;
            }
            throw v2;
          }
          v4[v1] = v3;
        }
        i = { type: e[0], nested: v4 };
      } else if (i["TAG"] === "Z") {
        let v5 = i["_0"],
          v9 = new Array(v5.length);
        for (let v6 = 0; v6 < v5.length; ++v6) {
          let v8;
          try {
            v8 = r0(v5[v6]);
          } catch (v7) {
            if (v7 && v7.s === s) {
              v7.path = '["_0"]' + '["' + v6 + '"]' + v7.path;
            }
            throw v7;
          }
          v9[v6] = v8;
        }
        i = { type: e[1], nested: v9 };
      }
    }
    return i;
  };
  return r0(i);
};

(i) => {
  return e[0](i);
};
(i) => {
  if (typeof i === "object" && i) {
    if (i["TAG"] === "A") {
      let v0 = i["_0"],
        v4 = new Array(v0.length);
      for (let v1 = 0; v1 < v0.length; ++v1) {
        let v3;
        try {
          v3 = e[0][0](v0[v1]);
        } catch (v2) {
          if (v2 && v2.s === s) {
            v2.path = '["_0"]' + '["' + v1 + '"]' + v2.path;
          }
          throw v2;
        }
        v4[v1] = v3;
      }
      i = { type: "A", nested: v4 };
    } else if (i["TAG"] === "Z") {
      let v5 = i["_0"],
        v9 = new Array(v5.length);
      for (let v6 = 0; v6 < v5.length; ++v6) {
        let v8;
        try {
          v8 = e[1][0](v5[v6]);
        } catch (v7) {
          if (v7 && v7.s === s) {
            v7.path = '["_0"]' + '["' + v6 + '"]' + v7.path;
          }
          throw v7;
        }
        v9[v6] = v8;
      }
      i = { type: "Z", nested: v9 };
    }
  }
  return i;
};
