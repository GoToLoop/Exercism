/// <reference path="./global.d.ts" />

// @ts-check

const PRICES = Object.freeze({ Margherita: 7, Caprese: 9, Formaggio: 10,
                               ExtraSauce: 1, ExtraToppings: 2 });

/**
 * Determines the price of the pizza given the pizza and optional extras.
 *
 * @param {Pizza} pizza name of the pizza to be made
 * @param {Extra[]} extras list of extras
 * @returns {number} the price of the pizza + extras
 */
export const pizzaPrice = (pizza, ...extras) => PRICES[pizza] + extra(extras);

/**
 * Recursively calculates the total price of the extras given a list of extras.
 * 
 * @param {Extra[]} tops list of extras for the pizza (toppings & sauces)
 * @returns {number} the total price of the extras
 */
const extra = (tops, $=PRICES[tops[0]]) => $ ? $ + extra(tops.slice(1)) : 0;

/**
 * Recursively calculates the price of the total order, given individual orders.
 *
 * @param {PizzaOrder[]} ords a list of pizza orders
 * @returns {number} the price of the total order
 */
export function orderPrice(ords, [o]=ords) {
  //return o ? pizzaPrice(o.pizza, ...o.extras) + orderPrice(ords.slice(1)) : 0;
  return ords.reduce(($, ord) => $ + pizzaPrice(ord.pizza, ...ord.extras), 0); }
