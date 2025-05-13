(i) => {
  if (i === null) {
    i = void 0;
  } else if (!(typeof i === "boolean" || i === void 0)) {
    e[0](i);
  }
  return i;
};
