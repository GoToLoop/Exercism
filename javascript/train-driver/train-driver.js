// @ts-check

/**
 * Return each wagon's id in the form of an array.
 *
 * @param {...number} ids - A list of wagon IDs
 * @returns {number[]} Array of wagon IDs
 */
export function getListOfWagons(...ids) { return ids; }

/**
 * Reorder the array of wagons by moving the first 2 wagons to the end.
 *
 * @param {Iterable<number>} ids - An iterable of wagon IDs
 * @returns {number[]} Reordered list of wagon IDs
 */
export const fixListOfWagons = ([a, b, ...rest]) => [...rest, a, b];

/**
 * Fixes the array of wagons by inserting an array of wagons after the first
 * element in the list of wagon IDs.
 *
 * @param {Iterable<number>} ids - Original list of wagon IDs
 * @param {Iterable<number>} miss - Missing wagon IDs to insert
 * @returns {number[]} Corrected list of wagon IDs
 */
export const correctListOfWagons = ([a, ...r], miss) => [a, ...miss, ...r];

/**
 * Extend route information by adding another object.
 *
 * @param {Record<string, string>} info - Original route information
 * @param {Record<string, string>} extra - Additional route details
 * @returns {Record<string, string>} Combined route information
 */
export const extendRouteInformation = (info, extra) => ({...info, ...extra});

/**
 * Separate arrival time from the route information object.
 *
 * @param {Record<string, string>} info - Route information object
 * @returns {[string, Record<string, string>]} Tuple with arrival time and
 *                                             remaining info
 */
export const separateTimeOfArrival = ({timeOfArrival: t, ...r}) => [t, r];
