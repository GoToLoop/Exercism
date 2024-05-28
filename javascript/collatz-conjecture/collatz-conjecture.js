// @ts-check

/** @param {number} n a positive integer */
export function steps(n, count=0) {
  if ((n |= 0) < 1) throw Error('Only positive numbers are allowed');
  while (n != 1) n = n & 1 && 3*n + 1 || n >> 1, ++count; return count; }
