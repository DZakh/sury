(i) => {
  if (typeof i === "object" && i) {
    if (i["type"] === "A") {
      try {
        let v0 = i["value"];
        if (!(typeof v0 === "string" && (v0 === "foo" || v0 === "bar"))) {
          e[0](v0);
        }
        i = { TAG: e[1], _0: v0 };
      } catch (e0) {
        try {
          let v1 = i["value"];
          if (typeof v1 !== "string") {
            e[2](v1);
          }
          i = { TAG: e[3], _0: v1 };
        } catch (e1) {}
      }
    } else {
      e[4](i);
    }
  } else {
    e[5](i);
  }
  return i;
};
