(i) => {
  if (i !== void 0 && i !== null && typeof i !== "string") {
    e[0](i);
  }
  let v1;
  if (i !== void 0) {
    let v0;
    if (i !== null) {
      v0 = i;
    } else {
      v0 = void 0;
    }
    v1 = v0;
  }
  return v1;
};

(i) => {
  if (!(typeof i === "string" || i === void 0 || i === null)) {
    e[0](i);
  }
  return i;
};
