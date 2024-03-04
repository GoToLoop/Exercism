// @ts-check

/**
 * The number of minutes it takes to prepare a single layer.
 */
const PREPARATION_MINUTES_PER_LAYER = 2;

/**
 * The expected oven time in minutes.
 */
export const EXPECTED_MINUTES_IN_OVEN = 40;

/**
 * Determines the number of minutes the lasagna still needs to remain in the
 * oven to be properly prepared.
 *
 * @param {number} actualMinutesInOven already elapsed baking time
 * @returns {number} the number of minutes remaining
 */
export function remainingMinutesInOven(actualMinutesInOven) {
  return EXPECTED_MINUTES_IN_OVEN - actualMinutesInOven;
}

/**
 * Given a number of layers, determines the total preparation time.
 *
 * @param {number} numberOfLayers the number of layers in the lasagna
 * @returns {number} the total preparation time
 */
export function preparationTimeInMinutes(numberOfLayers) {
  return PREPARATION_MINUTES_PER_LAYER * numberOfLayers;
}

/**
 * Calculates total working time. That is, the time to prepare all the layers
 * of lasagna, and the time already spent in the oven.
 *
 * @param {number} numberOfLayers the number of layers in the lasagna
 * @param {number} actualMinutesInOven elapsed cooking time
 * @returns {number} total working elapsed time in preparing and cooking
 */
export function totalTimeInMinutes(numberOfLayers, actualMinutesInOven) {
  return preparationTimeInMinutes(numberOfLayers) + actualMinutesInOven;
}
