// @ts-check

/**
 * Generates a random floating-point number within a specified range.
 *
 * @param {number} lo - The lower bound of the range (inclusive).
 * @param {number} hi - The upper bound of the range (exclusive).
 *
 * @returns {number} A random number between `lo` and `hi`.
 */
export function rndRng(lo, hi) { return Math.random() * (hi - lo) + lo; }

/**
 * Generates a random starship registry number.
 *
 * @returns {string} the generated registry number.
 */
export const randomShipRegistryNumber = () => 'NCC-' + ~~rndRng(1_000, 10_000);

/**
 * Generates a random stardate.
 *
 * @returns {number} a stardate between 41000 (inclusive) and 42000 (exclusive).
 */
export function randomStardate() { return rndRng(41_000, 42_000); }

/**
 * Generates a random planet class.
 *
 * @returns {string} a one-letter planet class.
 */
export const randomPlanetClass = () => 'DHJKLMNRTY'[ 10 * Math.random() | 0 ];
