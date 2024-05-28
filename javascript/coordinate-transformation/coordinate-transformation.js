// @ts-check

/**
 * This represents a function that takes a pair of coordinates as parameters and
 * returns a `TransformClosure`. The returned `TransformClosure` captures the 
 * provided pair of coordinates in its closure and uses them to perform a 2D 
 * transformation on another pair of coordinates that it takes as parameters.
 *
 * @callback TransformFunction
 *
 * @param {number} x fixed `x` coordinate of the first pair
 * @param {number} y fixed `y` coordinate of the first pair
 *
 * @returns {TransformClosure} the inner function that captures the coordinate
 * pair in its closure and uses them to transform another pair of coordinates
 */

/**
 * This represents a function that performs a 2D transformation on a pair of
 * coordinates. It is designed to be returned by another function, capturing
 * fixed parameters in a closure to perform those transformations. The function
 * takes a second pair of coordinates as parameters, which are then transformed
 * based on the fixed pair captured in the closure.
 *
 * @callback TransformClosure
 *
 * @param {number} x `x` coordinate of the second pair
 * @param {number} y `y` coordinate of the second pair
 *
 * @returns {[number, number]} the transformed coordinate pair
 */

/**
 * Constructs a function when invoked returns a `TransformClosure` that uses
 * closure to perform a repeatable 2D translation on a fixed coordinate pair.
 *
 * @param {number} dx fixed x component captured in the closure
 * @param {number} dy fixed y component captured in the closure
 *
 * @returns {TransformClosure} a function which takes x & y parameters and
 * returns the translated coordinate pair in the form [x, y] using closure
 */
export const translate2d = (dx, dy) => (x, y) => [ dx + x, dy + y ];

/**
 * Constructs a function that when invoked returns a `TransformClosure` that
 * uses closure to perform a repeatable 2D scale on a fixed coordinate pair.
 *
 * @param {number} sx scaling factor for the x component captured in the closure
 * @param {number} sy scaling factor for the y component captured in the closure
 *
 * @returns {TransformClosure} a function which takes x & y  parameters and
 * returns the scaled coordinate pair in the form [x, y] using closure
 */
export const scale2d = (sx, sy) => (x, y) => [ sx * x, sy * y ];

/**
 * Constructs a function composition that returns a combination of two functions
 * that perform repeatable transformations using captured `x` & `y` as closure.
 *
 * @param {TransformClosure} f the first function to apply
 * @param {TransformClosure} g the second function to apply
 *
 * @returns {TransformClosure} a function which takes an x, y parameter,
 * returns the transformed coordinate pair in the form [x, y]
 */
export const composeTransform = (f, g) => (x, y) => g( ...f(x, y) );

/**
 * Returns a function that memoizes the last result. If the arguments are the
 * same as the last call, then the memoized result is returned.
 *
 * @param {TransformClosure} f the transformation function to memoize,
 * assuming to take 2 arguments 'x' and 'y' and returning a tuple of 2 numbers
 * @param {string=} k an internal cached key parameter
 * @param {[number, number]=} v an internal cached value parameter
 *
 * @returns {TransformClosure} a function which takes 'x' and 'y' arguments,
 * and will either return the previous saved result if the arguments are the
 * same, or compute & cache a new result if they are different
 */
export const memoizeTransform = (f, k, v) => function (x, y) {
  // const key = x + ',' + y;
  const key = '' + [ ...arguments ];
  return key == k && v || ( k = key, v = f(x, y) ); };
