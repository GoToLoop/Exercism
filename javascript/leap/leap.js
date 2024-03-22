// @ts-check

/** @param {number} year */
export const isLeap = year => !( year % 100 ? year % 4 : year % 400 );
