/// <reference path="./global.d.ts" />
// @ts-check

/**
 * Get the first card in the given deck.
 * @param {Card[]} deck an array of cards, ordered from top to bottom
 * @returns {Card} the first card in the deck
 */
export const getFirstCard = ([first]) => first;

/**
 * Get the second card in the given deck.
 * @param {Card[]} deck an array of cards, ordered from top to bottom
 * @returns {Card} the second card in the deck
 */
export const getSecondCard = ([, second]) => second;

/**
 * Switch the position of the first two cards in the given deck.
 * @param {Card[]} deck an array of cards, ordered from top to bottom
 * @returns {Card[]} new deck with reordered cards
 */
export const swapTopTwoCards = ([one, two, ...rest]) => [two, one, ...rest];

/**
 * Put the top card of the given deck into a separate discard pile.
 * @param {Card[]} deck an array of cards, ordered from top to bottom
 * @returns {[Card, Card[]]} the top card of the given
 * deck and a new deck containing all the other cards
 */
export const discardTopCard = ([first, ...rest]) => [first, rest];

/** @type {Readonly<Card[]>} */
const FACE_CARDS = Object.freeze(['jack', 'queen', 'king']);

/**
 * Insert face cards into the given deck.
 * @param {Card[]} deck an array of cards, ordered from top to bottom
 * @returns {Card[]} new deck where 2nd, 3rd and 4th cards are face cards
 */
export const insertFaceCards = ([f, ...r]) => [f, ...FACE_CARDS, ...r];
