/**
 * Callback for array method reduce() to get total sum of bird visits.
 * 
 * @param {number} partialSum bird visits accumulator
 * @param {number} visitCount current number of bird sights in one day
 * @returns {number} partial sum of bird sights since logs started
 */
const birdVisitsCounter = (partialSum, visitCount) => partialSum + visitCount;

/**
 * Calculates the total bird count.
 *
 * @param {number[]} birdsPerDay log of daily bird sightings in the garden
 * @returns {number} total bird count since logs started
 */
export function totalBirdCount(birdsPerDay) {
  return birdsPerDay.reduce(birdVisitsCounter);
}

/**
 * Calculates the total number of birds seen in a specific week.
 *
 * @param {number[]} birdsPerDay log of daily bird sightings in the garden
 * @param {number} week a 1-based index representing a chunk of 7 indices
 * @returns {number} birds counted in the given week
 */
export function birdsInWeek(birdsPerDay, week) {
  return totalBirdCount(birdsPerDay.slice(7 * (week - 1), 7 * week));
}

/**
 * Fixes the untracked hidden bird mistake by increasing the bird count by one
 * in place for every second day (even indices).
 *
 * @param {number[]} birdsPerDay log of daily bird sightings in the garden
 * @returns {number[]} fixed bird count array, incremented for every even index
 */
export function fixBirdCountLog(birdsPerDay) {
  for (var i = 0; i < birdsPerDay.length; i += 2) ++birdsPerDay[i];
  return birdsPerDay;
}
