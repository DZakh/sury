(i) => {
  let v0 = i;
  if (typeof i !== "boolean") {
    if (i !== undefined) {
      e[2](i);
    } else {
      v0 = "undefined";
    }
  } else {
    v0 = "" + i;
  }
  return v0;
};
