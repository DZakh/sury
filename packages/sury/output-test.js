(i) => {
  if (typeof i === "string") {
    if (i === "t") {
      i = true;
    } else if (i === "1") {
      i = true;
    } else if (i === "f") {
      i = false;
    } else if (i === "0") {
      i = false;
    }
    try {
      let v0;
      (v0 = i === "true") || i === "false" || e[4](i);
      i = v0;
    } catch (e4) {
      e[5](i, e4);
    }
  } else {
    e[6](i);
  }
  return i;
};
