(i) => {
  try {
    if (!((typeof i === "number" && !Number.isNaN(i)) || i === void 0)) {
      e[0](i);
    }
    let v0 = e[1](i === void 0 ? -123 : i);
    if (typeof v0 !== "string") {
      e[2](v0);
    }
    i = v0;
  } catch (e0) {
    try {
      if (!((typeof i === "number" && !Number.isNaN(i)) || i === void 0)) {
        e[3](i);
      }
      let v1 = e[4](i === void 0 ? -123 : i);
      if (v1 !== void 0) {
        e[5](v1);
      }
      i = v1;
    } catch (e1) {
      e[6](i, e0, e1);
    }
  }
  return i === void 0 ? "not positive" : i;
};
