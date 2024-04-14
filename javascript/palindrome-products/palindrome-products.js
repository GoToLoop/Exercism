// @ts-check

/** @typedef {[number, number]} FactorPair */
/** @typedef {ReturnType<typeof factors>} Factors */

export const Palindromes = { generate({ minFactor: min=0, maxFactor: max=0 }) {
  if (min > max) throw Error('min must be <= max');

  const minProds = /** @type {FactorPair[]} */([]), maxProds = [...minProds];

  var minPal = Infinity, maxPal = -Infinity, prod = 0;

  for (var i = min; i <= max; ++i) for (var j = i; j <= max; ++j) {
    if (!isPalindrome(prod = i * j)) continue;

    if (prod < minPal) minPal = prod, minProds.length = 0;
    if (prod == minPal) { minProds.push([i, j]); continue; };

    if (prod > maxPal) maxPal = prod, maxProds.length = 0;
    if (prod == maxPal) maxProds.push([i, j]); }

  return palindromes(factors(minPal, minProds), factors(maxPal, maxProds)); } };

/** @param {number} v @param {FactorPair[]} factors */
const factors = (v, factors) => ({ value: factors.length ? v : null, factors });

/** @param {Factors} smallest @param {Factors} largest */
const palindromes = (smallest, largest) => ({ smallest, largest });

//const isPalindrome = n => [...n = ~~Math.abs(n) + ''].reverse().join('') == n;

function isPalindrome(n=0, ori=n = Math.abs(n | 0), rev=0) {
  if (n && (n % 10 == 0)) return false;
  while (n) rev = rev * 10 + n % 10, n = n / 10 | 0; return ori == rev; }
