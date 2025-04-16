(i) => {
  if (typeof i === "string") {
    if (i === "apple") {
      throw e[0];
    }
    try {
      throw e[1];
    } catch (e1) {
      e[2](i, e1);
    }
  } else {
    e[3](i);
  }
  return i;
};
