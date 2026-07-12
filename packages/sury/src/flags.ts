export type Flag = number;

export const flagNone: Flag = 0;
export const flagAsync: Flag = 1;
export const flagDisableNanNumberValidation: Flag = 2;
// flatten: 64

export const flagUnsafeHas = (acc: Flag, flag: Flag): boolean => {
  return (acc & flag) !== 0;
}

export const valFlagNone: Flag = 0;
export const valFlagAsync: Flag = 1;
