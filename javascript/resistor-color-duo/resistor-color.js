// @ts-check

/** @typedef {typeof COLORS[number]} Color */

/** @param {Color} color */
export const colorCode = color => COLORS.indexOf(color);

export const COLORS = Object.freeze(/** @type {const} */ ([
  'black', 'brown', 'red', 'orange', 'yellow',
  'green', 'blue', 'violet', 'grey', 'white' ]));
