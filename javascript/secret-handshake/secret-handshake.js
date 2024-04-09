// @ts-check

/** @typedef {typeof ACTIONS[keyof typeof ACTIONS]} Actions */

const ACTIONS = Object.freeze(
  { 1: 'wink', 2: 'double blink', 4: 'close your eyes', 8: 'jump' });

/**
 * Converts a secret number into a series of actions by applying a 5-bit mask.
 * @param {number} secret spoken value to be converted to a 5-bit value
 * @param {Actions[]} _actions private array to be populated with actions
 * @returns {Actions[]} an array of up to 4 actions
 */
export function commands(secret, _actions=[]) {
  for (const mask in ACTIONS) if (secret & +mask) _actions.push(ACTIONS[mask]);
  return secret & 16 && _actions.reverse() || _actions; }
