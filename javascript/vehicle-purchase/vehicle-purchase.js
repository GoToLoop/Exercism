/**
 * This tuple array represents the types of vehicles that require a license.
 * 
 * @type {Readonly<['car', 'truck']>}
 */
export const LICENSED_VEHICLE_TYPES = Object.freeze([ 'car', 'truck' ]);

/**
 * Determines whether you need a license to operate a certain kind of vehicle.
 *
 * @param {string} kind the vehicle type to check
 * @returns {boolean} whether or not a license is required for `kind`
 */
export function needsLicense(kind) {
  return LICENSED_VEHICLE_TYPES.includes( kind.trim().toLowerCase() );
}

/**
 * Advice message added to recommended vehicle
 */
const ADVICE = ' is clearly the better choice.';

/**
 * Helps choosing between two options by recommending the one that
 * comes first in dictionary order.
 *
 * @param {string} option1 the first vehicle option to consider
 * @param {string} option2 the second vehicle option to consider
 * @returns {string} a recommendation message indicating which 
 * vehicle option to choose based on alphabetical order
 */
export function chooseVehicle(option1, option2) {
  return (option1 <= option2 ? option1 : option2) + ADVICE;
}

/**
 * A map of car age to resell rate.
 * Key-value pairs denote age range (years) and resell rate. For instance:
 * - Cars aged >10 years resell at 50% of cost
 * - Cars aged from >=3 to <=10 years resell at 70% of cost
 * - Cars aged <3 years resell at 80% of cost
 */
export const AGE_TO_RESELL_PERCENTAGE = Object.freeze(new Map([
  [ 10.001, .5 ], // >10: 50%
  [ 3, .7 ], // >=3 to <=10: 70%
  [ 0, .8 ] // <3: 80%
]));

/**
 * Calculates an estimate for the price of a used vehicle in the dealership
 * based on the original price and the age of the vehicle.
 *
 * @param {number} originalPrice vehicle's brand new price
 * @param {number} age how old vehicle is
 * @returns {number} expected resell price in the dealership
 */
export function calculateResellPrice(originalPrice, age) {
  for (const [ range, rate ] of AGE_TO_RESELL_PERCENTAGE)
    if (age >= range) return originalPrice * rate;
}
