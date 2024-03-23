// @ts-check

/**
 * @param {string} a
 * @param {string} b
 * @throws {Error}
 */
export function compute(a, b) {
  if (a.length != b.length) throw Error('strands must be of equal length');
  for (var diff = 0, i = 0; i < a.length; diff += +(a[i] != b[i++]));
  return diff; }
