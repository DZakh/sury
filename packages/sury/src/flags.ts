export type Flag = number;

// Flag
export const flagNone: Flag = 0;
export const flagAsync: Flag = 1;
export const flagDisableNanNumberValidation: Flag = 2;
// flatten: 64
// let without = (flags, flag) => flags->with(flag)->Int.bitwiseXor(flag)

export const flagUnsafeHas = (acc: Flag, flag: Flag): boolean => {
  return (acc & flag) !== 0;
}

// ValFlag
export const valFlagNone = 0;
export const valFlagAsync = 1;
