// @ts-check

///////////////////////////// * Callback Types * /////////////////////////////

/**
 * This is a callback function that handles the result of an asynchronous
 * operation. It follows the Node.js convention of taking an error object as
 * the first argument and the result data as the second argument.
 *
 * @template T
 * @callback Handler
 *
 * @arg {Error?} error An Error object if an error occurred, null otherwise.
 * @arg {T=} data The data to be returned by the asynchronous operation.
 *
 * @returns {void} This callback does not return a value.
 */

/**
 * This function performs an asynchronous fetch operation. It takes an input
 * `value` and a `callback` function. When the fetch operation is complete,
 * it calls the `callback` function passing that `value` as its 2nd argument
 * by default, or some other `value`.
 *
 * @template T
 * @callback Fetcher
 *
 * @arg {T} value The input default value for the fetch operation.
 * @arg {Handler<T>} callback The callback to be called when the fetch
 * operation is complete.
 *
 * @returns {void} This function does not return a value.
 */

/**
 * This is a promisified version of a fetch operation. It takes an input
 * `value` and returns a Promise that resolves w/ the result of that `value`.
 *
 * @template T
 * @callback Promisified
 *
 * @arg {T} value The input value for the operation.
 * @returns {Promise<T>} Promise that resolves w/ the result of the operation.
 */

////////////////////////////// * promisify() * ///////////////////////////////

/**
 * This function takes a fetch operation and returns a promisified version of
 * that operation. The returned function takes the same input as the original
 * fetch operation and returns a Promise that resolves with the result of it.
 *
 * @template T
 * @param {Fetcher<T>} fetcher The fetch operation to be promisified.
 * @returns {Promisified<T>} The promisified version of the fetch operation.
 */
export function promisify(fetcher) {
  return value => new Promise((resolve, reject) => fetcher(value,
    (error, data=value) => error ? reject(error) : resolve(data))); }

///////////////////////////////// * all() * //////////////////////////////////

/**
 * Creates a Promise that is resolved with an array of results when all of
 * the provided Promises resolve, or rejected when any Promise is rejected.
 * 
 * @template T
 * @alias Promise.all
 * @overload
 *
 * @param {Iterable<T | PromiseLike<T>>} promises An array of promises.
 *
 * @returns {Promise<Awaited<T>[]>} A new Promise that resolves as an array of
 * results when all the `promises` in the provided array have been resolved.
 */

/**
 * @overload
 * @returns {Promise<undefined>}
 */

/**
 * @param {Iterable<T | PromiseLike<T>>} [promises] An array of promises.
 *
 * @returns {Promise<Awaited<T>[] | undefined>} A new Promise that resolves
 * as an array of results of type T when all the `promises` in the provided
 * array have been resolved.
 */
export function all(promises) {
  if (!promises) return VOID; // resolves as undefined

  const promiseArr = Object.freeze([...promises]), len = promiseArr.length,
        results = /** @type Awaited<T>[] */(Array(len).fill(null));

  if (!len) return Promise.resolve(results); // resolves as an empty array

  return new Promise((ok, bad, count=0) => {
    for (let i = 0; i < len; ++i) Promise.resolve(promiseArr[i]).then(val =>
      (results[i] = val, ++count == len && ok(results)), bad); }); }

////////////////////////////// * allSettled() * //////////////////////////////

/**
 * Creates a Promise that is resolved with an array of results when all of the
 * provided Promises resolve or reject.
 *
 * @template T
 * @alias Promise.allSettled
 * @overload
 *
 * @param {Iterable<T | PromiseLike<T>>} promises An array of Promises.
 *
 * @returns {Promise<Awaited<T | Error>[]>} A new Promise that resolves with
 * an array of results of type T or Error, when all of the provided `promises`
 * have been either resolved or rejected.
 */

/**
 * @overload
 * @returns {Promise<undefined>} A new Promise that resolves as undefined.
 */

/**
 * @param {Iterable<T | PromiseLike<T>>} [promises] An array of Promises.
 *
 * @returns {Promise<Awaited<T | Error>[] | undefined>} A new Promise that
 * resolves with an array of results of type T or Error, when all of the
 * provided `promises` have been either resolved or rejected.
 */
export function allSettled(promises) {
  if (!promises) return VOID; // resolves as undefined

  const promiseArr = Object.freeze([...promises]), len = promiseArr.length,
        results = /** @type Awaited<T | Error>[] */(Array(len).fill(null));

  if (!len) return Promise.resolve(results); // resolves as an empty array

  return new Promise((ok, _, count=0) => {
    for (let i = 0; i < len; ++i) Promise.resolve(promiseArr[i]).then(val =>
      (results[i] = val, ++count == len && ok(results)), err =>
      (results[i] = err, ++count == len && ok(results))); }); }

///////////////////////////////// * race() * /////////////////////////////////

/**
 * Creates a Promise that is resolved or rejected when any of the provided
 * Promises are resolved or rejected.
 *
 * @overload
 * @param {never[]} empty An empty array.
 * @returns {Promise<never[]>} A new Promise that resolves as an empty array.
 */

/**
 * @template T
 * @alias Promise.race
 * @overload
 *
 * @param {Iterable<T | PromiseLike<T>>} promises An array of promises.
 *
 * @returns {Promise<Awaited<T>>} A new Promise that is settled with the
 * value of the first Promise that is resolved or rejected.
 */

/**
 * @overload
 * @returns {Promise<undefined>} A new Promise that resolves as undefined.
 */

/**
 * @param {Iterable<T | PromiseLike<T>> | never[]} [promises]
 * An array or iterable of promises.
 *
 * @returns {Promise<Awaited<T> | never[] | undefined>} A new Promise that is
 * settled with the value of the first Promise that is resolved or rejected.
 */
export function race(promises) {
  if (!promises) return VOID; // resolves as undefined

  const promiseArr = [...promises], len = promiseArr.length;
  if (!len) return Promise.resolve(/** @type {never[]} */(promiseArr));

  return new Promise((ok, bad) => {
    for (const p of promiseArr) Promise.resolve(p).then(ok, bad); }); }

///////////////////////////////// * any() * //////////////////////////////////

/**
 * Creates a Promise that resolves when any of the provided promises
 * resolves, or rejects when all promises fail.
 *
 * @overload
 * @param {never[]} empty An empty array.
 * @returns {Promise<never[]>} A new Promise that resolves as an empty array.
 */

/**
 * @template T
 * @overload
 *
 * @param {Iterable<T | PromiseLike<T>>} promises An array of promises.
 *
 * @returns {Promise<Awaited<T> | Error[]>} A new Promise that is resolved
 * with the value of the first Promise that is resolved, or it's rejected
 * as an array of all of the failed `promises`.
 */

/**
 * @overload
 * @returns {Promise<undefined>} A new Promise that resolves as undefined.
 */

/**
 * @param {Iterable<T | PromiseLike<T> | never[]>} [promises]
 * An array or iterable of promises.
 *
 * @returns {Promise<Awaited<T> | Error[] | never[] | undefined>}
 * A new Promise that is resolved with the value of the first Promise that is
 * resolved, or it's rejected as an array of all of the failed `promises`.
 */
export function any(promises) {
  if (!promises) return VOID; // resolves as undefined

  const promiseArr = Object.freeze([...promises]), len = promiseArr.length,
        fails = /** @type Error[] */(Array(len).fill(null));

  if (!len) return Promise.resolve(fails); // resolves as an empty array

  return new Promise((ok, bad, count=0) => {
    for (let i = 0; i < len; ++i) Promise.resolve(promiseArr[i]).then(val =>
      ok(val), err => (fails[i] = err, ++count == len && bad(fails))); }); }

//////////////////////////////////////////////////////////////////////////////

const VOID = Object.freeze(Promise.resolve(void 0));

//////////////////////////////////////////////////////////////////////////////
