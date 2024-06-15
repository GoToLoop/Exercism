// @ts-check

/** @param {*} digits */
export const isArmstrongNumber = (n=0, digits=n + '', len=digits.length) =>
  n == Uint8Array.from(digits).reduce((sum, val) => sum + val ** len, 0);
