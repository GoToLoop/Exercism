/// <reference path="./global.d.ts" />
// @ts-check

/** @typedef {Error} BadRequest */

/** Communicates with the API from the outer space translators' servers. */
export class TranslationService {
  /**
   * Creates a new service.
   * @param {ExternalApi} api the translator API
   */
  constructor(api) { this.api = api; }

  /**
   * Attempts to retrieve the translation for the given `text` (free service).
   *
   * - Returns whichever translation can be retrieved, regardless of quality.
   * - Forwards any error from the translation API.
   *
   * @param {string} text word to translate to English
   * @returns {Promise<Awaited<string>>} the English translation as a `Promise`
   * @throws {BadRequest} when type of passed `text` isn't `string`
   */
  free(text) { return this.api.fetch(text).then(t => t.translation); }

  /**
   * Performs batch translation of the provided `texts` (free service).
   *
   * - Resolves all the translations (in the same order), if they all succeed.
   * - Rejects with the first error that is encountered.
   * - Rejects with a BatchIsEmpty error if no texts are given.
   *
   * @param {string[]} texts the array of texts to be translated
   * @returns {Promise<Awaited<string[]>>} A promise that resolves with an
   * array of translated texts
   * @throws {BadRequest} when `texts` array contains any non-`string` values
   */
  batch(texts) {
    if (!texts.length) return Promise.reject(new BatchIsEmpty);
    return Promise.all(texts.map(this.free, this)); }

  /**
   * Requests that the API make available the translation of the input `text`
   * (premium service).
   *
   * Note: The request service is flaky, and it may take up to three times for
   *       it to accept the request.
   *
   * @param {string} text word to request its translation to become available
   * @returns {Promise<Awaited<void>>} resolves to undefined if successful
   * @throws {BadRequest} when type of passed `text` isn't `string`
   */
  request(text, {api} = this, counter=0) {
    return new Promise((solve, fail) => api.request(text, function cb(err) {
      if (err) ++counter < 3 ? api.request(text, cb) : fail(err);
      else solve(err); })); }

  /**
   * Retrieves the translation for the given `text` (premium service).
   *
   * - Rejects with an error if the quality cannot be met.
   * - Requests a translation if the translation isn't available, then retries.
   *
   * @param {string} text word to translate to English
   * @param {number} quality threshold to reach so it's not rejected
   * @returns {Promise<Awaited<string>>} the English translation as a `Promise`
   * @throws {BadRequest} when type of passed `text` isn't `string`
   */
  premium(text, quality, {api} = this) {
    const checker = (/** @type {Translation} */t) => t.quality >= quality ?
      t.translation : Promise.reject(new QualityThresholdNotMet(text));

    return api.fetch(text).then(checker, _ => this.request(text).then(
      _ => api.fetch(text).then(checker))); }}

/**
 * This error is used to indicate a translation was found, but its quality does
 * not meet a certain threshold. Do not change the name of this error!
 */
export class QualityThresholdNotMet extends Error {
  /** @param {string} text */
  constructor(text) { super(text + ' translation below requested quality.'); }}

/**
 * This error is used to indicate the batch service was called without any
 * texts to translate (it was empty). Do not change the name of this error!
 */
export class BatchIsEmpty extends Error {
  constructor() { super('No texts found in the batch array.'); }}
