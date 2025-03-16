(i) => {
  try {
    i = i.then(e[1]);
  } catch (e0) {
    try {
    } catch (e1) {
      e[2]([e0, e1]);
    }
  }
  return Promise.resolve(i);
};
