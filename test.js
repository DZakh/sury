(i) => {
  let v0 = i["0"],
    v1 = i["1"],
    v2 = i["2"];
  if (v0 === void 0) {
    v0 = null;
  }
  if (v1 !== void 0) {
    e[1](v1);
  }
  if (v2 !== "bar") {
    e[2](v2);
  }
  return [v0, v1, v2];
};

(i) => {
  let v0 = i["0"],
    v1 = i["1"],
    v2 = i["2"];
  if (!(typeof v0 === "string")) {
    if (v0 === void 0) {
      v0 = null;
    }
  }
  if (v1 !== void 0) {
    e[1](v1);
  }
  if (v2 !== "bar") {
    e[2](v2);
  }
  return [v0, v1, v2];
};
