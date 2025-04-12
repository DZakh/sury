(i) => {
  if (typeof i === "object" && i && i["BS_PRIVATE_NESTED_SOME_NONE"] === 0) {
    i = void 0;
  } else if (i === void 0) {
    i = null;
  }
  return i;
};

(i) => {
  if (i === void 0) {
    i = null;
  } else if (
    typeof i === "object" &&
    i &&
    i["BS_PRIVATE_NESTED_SOME_NONE"] === 0
  ) {
    i = void 0;
  }
  return i;
};
