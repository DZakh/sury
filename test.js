(i) => {
  if (typeof i === "object" && i) {
    if (i["BS_PRIVATE_NESTED_SOME_NONE"] === 1) {
      i = void 0;
    } else if (i["BS_PRIVATE_NESTED_SOME_NONE"] === 0) {
      i = void 0;
    }
  }
  return i;
};
