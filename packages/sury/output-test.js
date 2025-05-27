(i) => {
  if (typeof i !== "object" || !i) {
    e[2](i);
  }
  let v0 = i["foo"];
  if (typeof v0 !== "string") {
    e[0](v0);
  }
  return { TAG: e[1], _0: v0 };
};

(i) => {
  if (typeof i !== "object" || !i) {
    e[3](i);
  }
  let v0 = i["foo"];
  if (typeof v0 !== "string") {
    e[0](v0);
  }
  let v1 = v0["foo"];
  if (typeof v1 !== "string") {
    e[1](v1);
  }
  return { TAG: e[2], _0: v1 };
};
