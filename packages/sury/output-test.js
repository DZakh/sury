(i) => {
  if (typeof i === "object" && i) {
    if (
      i["TAG"] === "A" &&
      typeof i["_0"] === "object" &&
      i["_0"] &&
      typeof i["_0"]["payload"] === "object" &&
      i["_0"]["payload"]
    ) {
      let v0 = i["_0"];
      let v1 = v0["payload"];
      i = v0;
    } else if (
      i["TAG"] === "B" &&
      typeof i["_0"] === "object" &&
      i["_0"] &&
      typeof i["_0"]["payload"] === "object" &&
      i["_0"]["payload"]
    ) {
      let v3 = i["_0"];
      let v4 = v3["payload"];
      i = v3;
    }
  }
  return i;
};
