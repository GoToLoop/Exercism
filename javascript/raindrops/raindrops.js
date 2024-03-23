// @ts-check

const NOISES = Object.freeze({ 3: 'Pling', 5: 'Plang', 7: 'Plong' });

/** @param {number} n */
export function convert(n, s='') {
  for (const prime in NOISES) n % +prime || (s += NOISES[prime]);
  return s || s + n; };
