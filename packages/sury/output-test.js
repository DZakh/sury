(i) => {
  if (typeof i === "object" && i) {
    let v0 = i["foo"];
    if (typeof v0 !== "string") {
      e[1](v0);
    }
    i = [v0];
  } else if (!(i instanceof e[0])) {
    e[2](i);
  }
  return i;
};
