// @ts-check

/**
 * Transposes an array of strings.
 * @param {string[]} texts an array of strings to transpose
 * @returns {string[]} the transposed array of strings
 */
export function transpose(texts) {
  const lens = toLens(texts), len = maxLen(lens), idx = idxOfLen(lens, len);

  /** @type {string[]} transposed array = length of biggest text */
  const transposed = Array(len > 0 && len || 0).fill(''), l = texts.length;

  for (var txt = '', ch = '', j = 0, i = 0; i < len; ++i) {
    while (j < l) if ( ch = texts[j++][i] ) txt += ch;
                  else if (i < idx || i < maxLen(lens.subarray(j))) txt += ' ';
    transposed[i] = txt, txt = '', j = 0; }

  return transposed; }

/**
 * Represents an array-like container with numeric values.
 * @typedef {ArrayLike<number> & Iterable<number>} NumArr
 */

/**
 * Gets the `length` properties of each string from `arr`.
 * @param {ArrayLike<string>} arr an array of strings
 */
const toLens = arr => Uint8Array.from(arr, getLen), getLen = arr => arr.length;

/**
 * Given an array of `length` values, finds which one is longest.
 * @param {NumArr} lens an array of `length` values
 * @returns {number} the biggest `length` value
 */
const maxLen = lens => Math.max(...lens);

/**
 * Given an array of `length` values, finds its index.
 * @param {NumArr} lens an array of `length` values
 * @param {number} len the `length` value to search for
 * @returns {number} the index of the last occurrence of `len` in `lens`
 */
const idxOfLen = (lens, len) => Array.prototype.lastIndexOf.call(lens, len);
