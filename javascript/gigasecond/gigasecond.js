// @ts-check

/**
 * Determines a new Date which is 1 gigasecond after the provided Date.
 * @param {Date} date the provided Date
 * @returns {Date} a new Date which is 1_000_000_000_000 milliseconds later
 */
export const gigasecond = date => new Date(+date + 1_000_000_000_000);
