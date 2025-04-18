(i) => {
  let v0 = i["condition"],
    v22;
  let r0 = (v0) => {
    if (typeof v0 === "object" && v0) {
      if (v0["TAG"] === "Connective") {
        let v1 = v0["TAG"],
          v2 = v0["_0"]["operator"],
          v3 = v0["_0"]["conditions"],
          v7 = new Array(v3.length);
        if (v1 !== "Connective") {
          e[0](v1);
        }
        if (v2 !== "or") {
          e[1](v2);
        }
        for (let v4 = 0; v4 < v3.length; ++v4) {
          let v6;
          try {
            v6 = r0(v3[v4]);
          } catch (v5) {
            if (v5 && v5.s === s) {
              v5.path = '["_0"]["conditions"]' + "[\"'+v4+'\"]" + v5.path;
            }
            throw v5;
          }
          v7[v4] = v6;
        }
        v0 = { type: e[2], value: v7 };
      } else if (v0["TAG"] === "Connective") {
        let v8 = v0["TAG"],
          v9 = v0["_0"]["operator"],
          v10 = v0["_0"]["conditions"],
          v14 = new Array(v10.length);
        if (v8 !== "Connective") {
          e[3](v8);
        }
        if (v9 !== "and") {
          e[4](v9);
        }
        for (let v11 = 0; v11 < v10.length; ++v11) {
          let v13;
          try {
            v13 = r0(v10[v11]);
          } catch (v12) {
            if (v12 && v12.s === s) {
              v12.path = '["_0"]["conditions"]' + "[\"'+v11+'\"]" + v12.path;
            }
            throw v12;
          }
          v14[v11] = v13;
        }
        v0 = { type: e[5], value: v14 };
      } else if (v0["TAG"] === "Comparison") {
        let v15 = v0["TAG"],
          v16 = v0["_0"]["operator"],
          v17 = v0["_0"]["values"];
        if (v15 !== "Comparison") {
          e[6](v15);
        }
        if (v16 !== "equal") {
          e[7](v16);
        }
        v0 = { type: e[8], value: v17 };
      } else if (v0["TAG"] === "Comparison") {
        let v18 = v0["TAG"],
          v19 = v0["_0"]["operator"],
          v20 = v0["_0"]["values"];
        if (v18 !== "Comparison") {
          e[9](v18);
        }
        if (v19 !== "greater-than") {
          e[10](v19);
        }
        v0 = { type: e[11], value: v20 };
      }
    }
    return v0;
  };
  try {
    v22 = r0(v0);
  } catch (v21) {
    if (v21 && v21.s === s) {
      v21.path = '["condition"]' + v21.path;
    }
    throw v21;
  }
  return { condition: v22 };
};
