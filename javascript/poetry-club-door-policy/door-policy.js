/**
 * Respond with the correct character, given the line of the
 * poem, if this were said at the front door.
 *
 * @param {string} line guard's recited acrostic poem line
 * @returns {string} first letter of `line`
 */
export function frontDoorResponse(line) {
  return line[0];
}

/**
 * Format the password for the front-door, given the response
 * letters.
 *
 * @param {string} word the letters you responded with before
 * @returns {string} the front door password capitalized properly
 */
export function frontDoorPassword(word) {
  return word[0].toUpperCase() + word.slice(1).toLowerCase();
}

/**
 * Respond with the correct character, given the line of the
 * poem, if this were said at the back door.
 *
 * @param {string} line guard's recited telestich poem line
 * @returns {string} last letter of trimmed `line`
 */
export function backDoorResponse(line) {
  return line.trimEnd().at(-1);
}

/**
 * Format the password for the back door, given the response
 * letters.
 *
 * @param {string} word the letters you responded with before
 * @returns {string} the back door password capitalized properly w/ politeness
 */
export function backDoorPassword(word) {
  return frontDoorPassword(word) + POLITENESS;
}

const POLITENESS = ', please';
