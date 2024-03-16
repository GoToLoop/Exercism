/// <reference path="./global.d.ts" />
// @ts-check

import { notify } from './notifier';
import { order } from './grocer';

const SUCCESS = Object.freeze( { message: 'SUCCESS' } ),
      ERROR = Object.freeze( { message: 'ERROR' } );

export const onSuccess = () => notify(SUCCESS), onError = () => notify(ERROR);

export const orderFromGrocer = order;

/**
 * @param {string} variety
 * @param {number} quantity
 * @return void
 */
export const postOrder = (variety, quantity) =>
  orderFromGrocer({ variety, quantity }, onSuccess, onError);
