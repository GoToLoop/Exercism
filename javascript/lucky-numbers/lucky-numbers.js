// @ts-check

/**
 * Sums two numbers represented as arrays of digits (0-9).
 *
 * @param {number[]} array1 First number's digits.
 * @param {number[]} array2 Second number's digits.
 * @returns {number} The sum of the two numbers formed out of the input arrays.
 */
export const twoSum = (array1, array2) => +array1.join('') + +array2.join('');

/**
 * Checks whether a number is a palindrome.
 *
 * @param {number} n Number to check if it's a palindrome.
 * @returns {boolean} Whether the number is a palindrome or not.
 */
export const luckyNumber = n => n == +[... '' + n].reverse().join('');

/**
 * Determines the error message to be shown to the user based on the input.
 *
 * @param {string | null | undefined} input The `input` value from the user.
 * @returns {string} The error message to be displayed.
 *
 * @description If the `input` is null, undefined or an empty string, returns
 * 'Required field'. If the `input` can be converted to a non-zero number,
 * returns an empty string. Otherwise, if the `input` is zero, NaN or
 * a non-numeric string, returns 'Must be a number besides 0'.
 */
export const errorMessage = input => !input? 'Required field' :
                                     +input? '' : 'Must be a number besides 0';
