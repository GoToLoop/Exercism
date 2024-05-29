// @ts-check

/**
 * Determine how many cards of a certain type there are in the deck.
 *
 * @param {number[]} stack Elyse's deck
 * @param {number} type type of card to count
 *
 * @returns {number} number of cards of a single type there are in the deck
 */
export const cardTypeCheck = (stack, type, c=0) =>
  ( stack.forEach( card => card == type && ++c ), c );

/**
 * Determine how many cards are odd or even.
 *
 * @param {number[]} stack Elyse's deck
 * @param {boolean} checkForEven whether the check is for even or odd cards
 *
 * @returns {number} number of cards that are either odd or even
 */
export function determineOddEvenCards(stack, checkForEven, c=0) {
  for (const card of stack) card & 1 ^ +checkForEven && ++c; return c; }
