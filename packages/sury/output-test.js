(i) => {
  try {
    e[0](i);
  } catch (e0) {
    if (i === void 0) {
      i = null;
    }
  }
  return i;
};
