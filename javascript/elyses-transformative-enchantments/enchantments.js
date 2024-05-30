// @ts-check

/**
 * Double every card in the deck.
 * @param {number[]} deck
 * @returns {number[]} a new deck with every card doubled
 */
export const seeingDouble = deck => deck.map(card => card << 1);

/**
 * Creates triplicates of every 3 found in the deck.
 * @param {number[]} deck
 * @returns {number[]} deck with triplicate threes
 */
export function threeOfEachThree(deck, idx=0, threes=Object.freeze([3, 3, 3])) {
  //while ( ~(idx = deck.indexOf(3, idx)) ) deck.splice(idx, 0, 3, 3), idx += 3;
  //return deck; }
  return deck.flatMap(card => card == 3 && threes || card); }

/**
 * Extracts the middle two cards from a deck. Assumes a deck is always 10 cards.
 * @param {number[]} deck of 10 cards
 * @returns {number[]} new deck with only two middle cards
 */
export const middleTwo = deck => deck.slice(4, 6);

/**
 * Moves the outside two cards (tail/bottom & head/top) to the middle in place.
 * @param {number[]} deck with even number of cards
 * @returns {number[]} transformed deck
 */
export const sandwichTrick = deck => deck.splice((deck.length >> 1) - 1, 0,
  deck.pop() || 0, deck.shift() || 0) && deck;

/**
 * Removes every card from the deck except 2s.
 * @param {number[]} deck
 * @returns {number[]} new deck with only twos
 */
export const twoIsSpecial = deck => deck.filter(card => card == 2);

/**
 * Returns a perfectly order deck from lowest to highest in place.
 * @param {number[]} deck shuffled deck
 * @returns {number[]} ordered deck
 */
export const perfectlyOrdered = deck => deck.sort((a, b) => a - b);

/**
 * Reorders in place the deck so that the top card ends up at the bottom.
 * @param {number[]} deck
 * @returns {number[]} same reordered deck in reverse
 */
export const reorder = deck => deck.reverse();
