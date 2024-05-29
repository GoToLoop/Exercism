// @ts-check

/**
 * Retrieve card from `cards` array at the 0-based `position`.
 *
 * @param {number[]} cards array of card numbers ranging from 1 to 10
 * @param {number} position index of the desired card within the array
 * @returns {number} the card number from a stack of 1 to 10 range
 */
export const getItem = (cards, position) => cards[position];

/**
 * Exchange card with `replacementCard` at the 0-based `position`.
 *
 * @param {number[]} cards array of card numbers ranging from 1 to 10
 * @param {number} position index of the card to be replaced within the array
 * @param {number} replacementCard card to replace current one at `position`
 * @returns {number[]} same `cards` array with the replacement change applied
 */
export const setItem = (cards, position, replacementCard) =>
  (cards[position] = replacementCard, cards);

/**
 * Insert `newCard` at the end of the `cards` array.
 *
 * @param {number[]} cards array of card numbers ranging from 1 to 10
 * @param {number} newCard card number to be added at `cards` array's tail
 * @returns {number[]} the same `cards` array with the `newCard` appended
 */
export const insertItemAtTop = (cards, newCard) => (cards.push(newCard), cards);

/**
 * Remove the card at the 0-based `position`.
 *
 * @param {number[]} cards array of card numbers ranging from 1 to 10
 * @param {number} position index of the card number to be removed
 * @returns {number[]} the same `cards` array without the removed card
 */
export const removeItem = (cards, position) =>
  (cards.splice(position, 1), cards);

/**
 * Remove card from the end of the `cards` array.
 *
 * @param {number[]} cards array of card numbers ranging from 1 to 10
 * @returns {number[]} the same `cards` array without the removed tail card
 */
export const removeItemFromTop = cards => (cards.pop(), cards);

/**
 * Insert `newCard` at beginning of the `cards` array.
 *
 * @param {number[]} cards array of card numbers ranging from 1 to 10
 * @param {number} newCard card number to be added at `cards` array's head
 * @returns {number[]} the same `cards` array with the `newCard` as 1st item
 */
export const insertItemAtBottom = (cards, newCard) =>
  (cards.unshift(newCard), cards);

/**
 * Remove card from the beginning of the `cards`.
 *
 * @param {number[]} cards array of card numbers ranging from 1 to 10
 * @returns {number[]} the same `cards` array without the removed head card
 */
export const removeItemAtBottom = cards => (cards.shift(), cards);

/**
 * Compare the number of `cards` with the given `stackSize`.
 *
 * @param {number[]} cards array of card numbers ranging from 1 to 10
 * @param {number} stackSize the size to check
 * @returns {boolean} true if there are exactly `stackSize` number of `cards`
 */
export const checkSizeOfStack = (cards, stackSize) => cards.length == stackSize;
