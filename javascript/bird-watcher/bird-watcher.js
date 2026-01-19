// @ts-check

/**
 * Callback for array method reduce() to get total sum of bird visits.
 * 
 * @param {number} partialSum - bird visits accumulator
 * @param {number} visitCount - current number of bird sights in one day
 * @returns {number} partial sum of bird sights since logs started
 */
const birdVisits = (partialSum, visitCount) => partialSum + visitCount;

/**
 * Calculates the total bird count.
 *
 * @param {ArrayLike<number>} birds - log of daily bird sightings in the garden
 * @returns {number} total bird count since logs started
 */
export const totalBirdCount = birds => Array.from(birds).reduce(birdVisits);

/**
 * Calculates the total number of birds seen in a specific week.
 *
 * @param {ArrayLike<number>} birds - log of daily bird sightings in the garden
 * @param {number} week - a 1-based index representing a chunk of 7 indices
 * @returns {number} birds counted in the given week
 */
export function birdsInWeek(birds, week) {
  return totalBirdCount(Array.from(birds).slice(7 * (week - 1), 7 * week)); }

/**
 * Fixes the untracked hidden bird mistake by increasing the bird count by one
 * in place for every second day (even indices).
 *
 * @param {number[]} birds - log of daily bird sightings in the garden
 */
export function fixBirdCountLog(birds) {
  for (var i = 0; i < birds.length; i += 2) ++birds[i]; }
