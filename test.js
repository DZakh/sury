(i) => {
  if (i === null) {
    i = e[0];
  } else if (i === void 0) {
    i = { BS_PRIVATE_NESTED_SOME_NONE: e[1] };
  } else if (!(typeof i === "boolean")) {
    e[2](i);
  }
  return i;
};
