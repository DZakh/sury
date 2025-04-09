(i) => {
  if (i === null) {
    i = { BS_PRIVATE_NESTED_SOME_NONE: 0 };
  } else if (!(typeof i === "boolean" || i === void 0)) {
    e[0](i);
  }
  return i;
};
