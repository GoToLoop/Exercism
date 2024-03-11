/// <reference path="./global.d.ts" />
// @ts-check

/**
 * Creates a new score board with an initial entry.
 *
 * @returns {Record<string, number>} new score board
 */
export function createScoreBoard() {
  return { 'The Best Ever': 1_000_000 };
}

/**
 * Adds a player to a score board.
 *
 * @param {Record<string, number>} scoreBoard existing { `player`: `score` }
 * @param {string} player name to be created/updated as key of `scoreBoard`
 * @param {number} score highest point mapped to `player`
 * @returns {Record<string, number>} updated score board
 */
export function addPlayer(scoreBoard, player, score) {
  scoreBoard[player] = score;
  return scoreBoard;
}

/**
 * Removes a player from a score board.
 *
 * @param {Record<string, number>} scoreBoard existing { `player`: `score` }
 * @param {string} player name whose key is to be deleted from `scoreBoard`
 * @returns {Record<string, number>} updated score board
 */
export function removePlayer(scoreBoard, player) {
  delete scoreBoard[player];
  return scoreBoard;
}

/**
 * Increases a player's score by the given amount.
 *
 * @param {Record<string, number>} scoreBoard existing { `player`: `score` }
 * @param {string} player name whose score should be increased
 * @param {number} points value to be added to the score
 * @returns {Record<string, number>} updated score board
 */
export function updateScore(scoreBoard, player, points) {
  scoreBoard[player] += points;
  return scoreBoard;
}

/**
 * Applies 100 bonus points to all players on the board.
 *
 * @param {Record<string, number>} scoreBoard existing { `player`: `score` }
 * @returns {Record<string, number>} updated score board
 */
export function applyMondayBonus(scoreBoard) {
  for (const player in scoreBoard) updateScore(scoreBoard, player, 100);
  return scoreBoard;
}

/**
 * Normalizes a score with the provided normalization function.
 *
 * @param {Params} params the parameters for performing the normalization
 * @returns {number} normalized score
 */
export function normalizeScore(params) {
  return params.normalizeFunction(params.score);
}
