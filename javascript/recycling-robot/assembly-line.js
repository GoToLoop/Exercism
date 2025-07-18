// @ts-check

import { ElectronicDevice } from './lib.js'; const { hasOwn } = Object;

/**
 * Checks if input is a boolean.
 *
 * @param {unknown} value
 * @returns {boolean} whether the input is a boolean
 */
export function isBoolean(value) { return typeof value == 'boolean'; }

/**
 * Checks if input is a finite number or bigint.
 *
 * @param {any} v
 * @returns {boolean} whether the input is a finite number or bigint
 */
export function isNumber(v) {
  const t = typeof v; return  t == 'number' && isFinite(v) || t == 'bigint'; }

/**
 * Checks if a value is an object.
 *
 * @param {unknown} v
 * @returns {boolean} whether the input is an object.
 */
export function isObject(v) { return v != null && typeof v == 'object'; }

/**
 * Checks if a value is a numeric string.
 *
 * @param {unknown} v
 * @returns {boolean} whether the input is a numeric string.
 */
export function isNumericString(v) {
  return v != '' && typeof v == 'string' && isFinite(+v); }

/**
 * Checks if an object is an instance of the `ElectronicDevice` class or one of
 * its children.
 *
 * @param {object} obj
 * @returns {boolean} whether the object is an instance of the class
 *                    `ElectronicDevice` or one of its children.
 */
export function isElectronic(obj) { return obj instanceof ElectronicDevice; }

/**
 * Checks if a value is a non empty array.
 *
 * @param {unknown} v
 * @returns {boolean} whether the input is a non empty array.
 */
export function isNonEmptyArray(v) { return Array.isArray(v) && v.length > 0; }

/**
 * Checks if a value is an empty array.
 *
 * @param {unknown} v
 * @returns {boolean} whether the input is an empty array.
 */
export function isEmptyArray(v) { return Array.isArray(v) && !v.length; }

/**
 * Checks if a value has a "type" property or method.
 *
 * @param {object} object
 * @returns {boolean} whether the input has a "type" property or method.
 */
export function hasType(object) { return 'type' in object; }

/**
 * Throws an error if an object is missing an "id" property or method.
 *
 * @param {object} object
 * @returns {never|void} undefined if the input has an "id" property or method,
 *                       otherwise throws an error.
 */
export function assertHasId(object) { if (!('id' in object)) throw new Error; }

/**
 * Checks if a value has an "id" property.
 *
 * @param {object} object
 * @returns {boolean} whether the input has an "id" property.
 */
export function hasIdProperty(object) { return hasOwn(object, 'id'); }

/**
 * Checks if a value has a defined "type" property.
 *
 * @param {object} obj
 * @returns {boolean} whether the input has a defined "type" property.
 */
export const hasDefinedType = obj => hasOwn(obj, 'type') && obj.type !== void 0;
