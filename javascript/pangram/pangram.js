// @ts-check

/**
 * Checks if sentence is using each 1 of the 26 letters at least once.
 * @param {string} s the input text (case insensitive) to check
 * @returns {boolean} whether the given text is a pangram or not
 */
export const isPangram = s => new Set(s.toLowerCase().match(/[a-z]/g)).size==26;
