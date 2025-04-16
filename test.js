(i) => {
  if (typeof i === "number") {
    if (i === 2) {
      i = e[0](i);
    } else if (!(i === 3)) {
      e[1](i);
    }
  } else {
    e[2](i);
  }
  return Promise.resolve(i);
};
