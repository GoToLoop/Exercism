// @ts-check

/**
 * Builds a sign for a specific `occasion` including the given `name`.
 *
 * @param {string} occasion The occasion for the sign
 * (e.g., “Birthday,” “Anniversary”).
 * @param {string} name The name to include in the sign.
 *
 * @returns {`Happy ${string} ${string}!`} A template string combining the
 * `occasion` and the `name`.
 */
export const buildSign = (occasion, name) => `Happy ${occasion} ${name}!`;

/**
 * Builds a birthday sign with conditional formatting based on the person’s age.
 *
 * @param {number} age The person’s age.
 *
 * @returns {`Happy Birthday! What a ${'mature' | 'young'} fellow you are.`}
 * A template string with a birthday message based on the `age`.
 */
export const buildBirthdaySign = age =>
  `Happy Birthday! What a ${age >= 50 && 'mature' || 'young'} fellow you are.`;

/**
 * Builds a multiline graduation sign for a specific person and graduation year.
 *
 * @param {string} name The graduate’s name.
 * @param {number} year The graduation year.
 *
 * @returns {`Congratulations ${string}!\nClass of ${number}`}
 * A multiline template string congratulating the graduate.
 */
export const graduationFor = (name, year) => `Congratulations ${name}!
Class of ${year}`;

/**
 * Determines the cost of creating a sign based on the number of characters in
 * the sign.
 *
 * @param {string} sign The sign content.
 * @param {string} currency The plural of the currency type
 * (e.g., “dollars”, “euros”).
 *
 * @returns {`Your sign costs ${number}.00 ${string}.`}
 * A template string informing the calculated cost to create the `sign` in the
 * specified `currency` based on the number of characters of that `sign`.
 */
export const costOf = (sign, currency) =>
  `Your sign costs ${sign.length * 2 + 20}.00 ${currency}.`;
