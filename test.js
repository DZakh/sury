(i) => {
  if (!(i !== 0)) {
    i = e[0](i);
  } else if (i !== 1) {
    e[1](i);
  }
  return Promise.resolve(i);
};

(i) => {
  if (typeof i === "number" && i === 1) {
    if (i === 0) {
      i = e[0](i);
    }
  } else {
    e[1](i);
  }
  return Promise.resolve(i);
};
