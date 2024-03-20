// @ts-check

import { colorCode } from './resistor-color';

/** @param {import('./resistor-color').Color[]} c */
export const decodedValue = (c) => +( '' + colorCode(c[0]) + colorCode(c[1]) );
