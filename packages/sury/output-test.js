(i) => {
  if (typeof i === "string") {
    let v0;
    try {
      v0 = BigInt(i);
    } catch (_) {
      e[0](i);
    }
    i = v0;
  } else if (typeof i === "number" && !Number.isNaN(i)) {
    i = BigInt(i);
  } else if (typeof i === "boolean") {
    throw e[1];
  } else {
    e[2](i);
  }
  if (typeof i !== "bigint") {
    e[3](i);
  }
  return i;
};
