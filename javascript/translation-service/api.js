// @ts-check

import { AbusiveClientError, NotAvailable, Untranslatable } from './errors';

const mutex = { current: false }; // flags if client is forever banned

/**
 * A dictionary that associates a foreign word key with a queue array (FIFO)
 * of translations + quality reliability percentages.
 * @typedef {Record<string, Array<?Translation>>} TranslatableValues
 */

/**
 * @typedef {Object} Translation Represents a translation of a foreign word.
 * @property {string} translation the translated text
 * @property {number} quality the reliability percentage for the translation
 */

/** API for the outer space translators' servers. */
export class ExternalApi {
  /** @param {TranslatableValues} [values={}] */
  constructor(values = {}) {
    /**
     * An object associating a word with a queue (FIFO) of its translations.
     * @protected
     * @type {TranslatableValues}
     */
    this.values = JSON.parse(JSON.stringify(values));
  }

  /**
   * Adds a `Translation` for a given word with a given quality into a queue.
   *
   * @param {string} value the foreign word as a property name
   * @param {string?} translation a new translation to append to queue's tail
   * @param {number} [quality=0] reliability percentage for the translation
   * @returns {this} chainable
   */
  register(value, translation, quality=0) {
    if (this.values[value] === void 0) this.values[value] = []; // new queue
    this.values[value].push(translation ? { translation, quality } : null);
    return this;
  }

  /**
   * Fetches 1st `Translation` object found for a given `text`.
   *
   * @param {string} text the foreign word to look up a translation for it
   * @returns {Promise<Awaited<Translation>>} the 1st found translation
   * @throws {BadRequest} when type of passed `text` isn't `string`
   */
  fetch(text) {
    if (typeof text !== 'string') throw new BadRequest(
      `Expected text when calling fetch(text), actual ${typeof text}.`,
    );

    // Checks if the client is currently banned from the API's servers:
    if (mutex.current) return rejectWithRandomDelay(new AbusiveClientError());

    const queue = this.values[text]; // translations for the same `text` key

    // Checks if 1st entry (head) has an actual `Translation` and resolves it:
    if (queue?.[0]) return resolveWithRandomDelay(queue[0]); // non-`null`

    // Word is found in the API, but its queue's 1st entry (head) is `null`:
    if (queue) return rejectWithRandomDelay(new NotAvailable(text));

    // Requested word doesn't even exist, so it's rejected as `Untranslatable`:
    return rejectWithRandomDelay(new Untranslatable());
  }

  /**
   * Searches the register for a `text` key and then attempts to find & make
   * available a valid `Translation` that's already stored in its queue array.
   *
   * @param {string} text word to request its translation to become available
   * @param {(err?: Error) => void} callback gets an `Error` for failure or
   * `undefined` for success
   * @throws {BadRequest} when type of passed `text` isn't `string` or type of
   * passed `callback` isn't `function`
   */
  request(text, callback) {
    if (typeof text !== 'string') throw new BadRequest(
      'Expected string text when calling request(text, callback), ' +
      `actual ${typeof text}.`,
    );

    if (typeof callback !== 'function') throw new BadRequest(
      'Expected callback function when calling request(text, callback), ' + 
      `actual ${typeof callback}.`,
    );

    const queue = this.values[text]; // translations for the same `text` key

    if (queue?.[0]) { // translation for `text` is already stored at index 0
      mutex.current = true; // client is now banned from the outer space API
      callback(new AbusiveClientError()); // for retrying an enabled entry 
    }

    else if (queue) { // `text` exists, but no `Translation` yet at index 0
      queue.shift(); // removes the `null` entry at the head of the queue

      // If next entry is `Translation` pass `undefined`, an `Error` otherwise:
      setTimeout(callback, 1, queue[0] ? void 0 : makeRandomError());
    }

    else callback(new Untranslatable()); // `text` hasn't been registered yet
  }
}

/**
 * Creates a `Promise` and resolves it with a random delay for a `Translation`.
 *
 * @param {Translation} value the `Translation` to be "promisified"
 * @returns {Promise<Awaited<Translation>>} A `Promise` that resolves with the
 * given `Translation` after a random delay
 */
function resolveWithRandomDelay(value) {
  const timeout = Math.random() * 100;
  return new Promise(resolve => setTimeout(resolve, timeout, value));
}

/**
 * Creates a rejected `Promise` after a random delay for a custom `Error`.
 *
 * @template {Error} E
 * @param {E} err an `Error` or a subtype of it to be rejected
 * @returns {Promise<never>} a `Promise` that rejects with the given error
 * after a random delay
 */
function rejectWithRandomDelay(err) {
  const timeout = Math.random() * 100;
  return new Promise((_, reject) => setTimeout(reject, timeout, err));
}

/**
 * Creates a new `Error` with a random error code.
 *
 * @returns {Error} an `Error` object with a random error code.
 */
function makeRandomError() {
  return Error('Error code ' + ~~(Math.random() * 10_000));
}

/** Represents an error when a request is made with a wrong argument type. */
class BadRequest extends Error { constructor(message) { super(message); } }
