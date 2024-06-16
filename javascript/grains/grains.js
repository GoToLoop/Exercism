// @ts-check

export function square(n=0, nn=0n) {
  if ((n |= 0) < 1 || n > 64) throw Error('square must be between 1 and 64');
  return console.log(n, nn = 1n << BigInt(n - 1)), nn; }

export const total = () => (1n << 64n) - 1n;
