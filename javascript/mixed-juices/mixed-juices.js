// @ts-check

/**
 * Determines how long it takes to prepare a certain juice.
 *
 * @param {string} name juice mix drink name
 * @returns {number} time in minutes
 */
export function timeToMixJuice(name) {
  switch (name) {
    case 'Pure Strawberry Joy': return .5;
    case 'Energizer':
    case 'Green Garden': return 1.5;
    case 'Tropical Island': return 3;
    case 'All or Nothing': return 5;
    default: return 2.5;
  }
}

/**
 * Represents the size of a lime. It can be 'small', 'medium', or 'large'.
 * @typedef {keyof typeof SIZES_TO_WEDGES} Size
 */

/** How many lime wedges that can be obtained based on its size. */
export const SIZES_TO_WEDGES = Object.freeze({
  'small': 6, 'medium': 8, 'large': 10
});

/**
 * Calculates the number of limes that need to be cut to reach a certain supply.
 *
 * @param {number} wedgesNeeded number of lime wedges needed for the entire day
 * @param {Size[]} limes available supply of limes of 3 different sizes
 * @returns {number} estimated number of limes to cut
 */
export function limesToCut(wedgesNeeded, limes) {
  var limesCut = 0, wedgesObtained = 0;

  if (wedgesNeeded) for (const size of limes) {
    ++limesCut;
    if ((wedgesObtained += SIZES_TO_WEDGES[size]) >= wedgesNeeded) break;
  }

  return limesCut;
}

/**
 * Determines which juices still need to be prepared after the end of the shift.
 *
 * @param {number} timeLeft minutes till Li Mei's shift ends (3 PM)
 * @param {string[]} orders juices that have been ordered but not prepared yet
 * @returns {string[]} remaining orders for Dmitry to prepare in his own shift
 */
export function remainingOrders(timeLeft, orders) {
  return orders.filter(juice => {
    if (timeLeft <= 0) return true;
    timeLeft -= timeToMixJuice(juice);
  });
  //return orders.filter(mix => timeLeft <= !(timeLeft -= timeToMixJuice(mix)));
}
