// ts-check

/** @param {string} color */
export const colorCode = color => COLORS.indexOf(color.toLowerCase());

export const COLORS = Object.freeze( /** @type {const} */ ([
  'black',  // 0
  'brown',  // 1
  'red',    // 2
  'orange', // 3
  'yellow', // 4
  'green',  // 5
  'blue',   // 6
  'violet', // 7
  'grey',   // 8
  'white'   // 9
]) );
