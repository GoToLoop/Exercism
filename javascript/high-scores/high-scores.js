// @ts-nocheck

/**
 * @constructor
 * @param {number[]} scores
 */
export function HighScores(scores) {
  this.latest = (this.scores = scores.slice()).at(-1);
  this.personalBest = Math.max(...scores);
  this.personalTopThree = scores.toSorted((a, b) => b - a).slice(0, 3); }
