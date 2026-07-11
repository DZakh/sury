// Path

export type Path = string;

export const pathEmpty: Path = "";
export const pathDynamic: Path = "[]";

export const inlinedValueFromString = (str: string): string => {
  for (let idx = 0; idx < str.length; idx++) {
    const ch = str[idx];
    if (ch === '"' || ch === "\n") return JSON.stringify(str);
  }
  return `"${str}"`;
}

export const pathFromInlinedLocation = (inlinedLocation: string): Path => {
  return `[${inlinedLocation}]`;
}

export const pathFromLocation = (location: string): Path => {
  return `[${inlinedValueFromString(location)}]`;
}

export const pathToArray = (path: Path): string[] => {
  switch (path) {
    case "":
      return [];
    default:
      return JSON.parse(path.split(`"]["`).join(`","`)) as string[];
  }
}

export const pathFromArray = (array: string[]): Path => {
  switch (array.length) {
    case 0:
      return "";
    case 1:
      return pathFromLocation(array[0]!);
    default:
      return array.map(pathFromLocation).join("");
  }
}

export const pathConcat = (path: Path, concatedPath: Path): Path => {
  return path + concatedPath;
}
