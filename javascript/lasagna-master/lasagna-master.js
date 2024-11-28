/// <reference path="./global.d.ts" />
// @ts-check

/**
 * Determines the cooking status of the lasagna based on the timer value.
 *
 * @param {number=} timer - The time left on the cooking timer.
 * If no value is provided, it is assumed that the timer was not set.
 *
 * @returns {string} A message indicating the cooking status.
 *
 * @description If the timer is 0, it returns: 'Lasagna is done.'
 * If no timer is set, it returns: 'You forgot to set the timer.'
 * Otherwise, it returns: 'Not done, please wait.'
 */
export const cookingStatus = timer => timer == 0 ? 'Lasagna is done.' :
  !timer && 'You forgot to set the timer.' || 'Not done, please wait.';

/**
 * Calculates the total time in minutes required to prepare the lasagna.
 *
 * @param {string[]} layers - Layers of ingredients to be added to the lasagna.
 *
 * @param {number} time - Time in minutes it takes to prepare a single layer.
 * If no value is provided, it's assumed each layer takes 2 minutes to prepare.
 *
 * @returns {number} Total preparation time in minutes.
 *
 * @description This function multiplies the number of `layers` by the time
 * it takes to prepare a single layer to calculate the total preparation time.
 */
export const preparationTime = (layers, time=2) => layers.length * time;

/** Each noodle layer weighs 50 grams and each sauce layer is 0.2 liters. */
const QUANTITIES = Object.freeze({ noodles: 50, sauce: .2 });

/**
 * Calculates the total quantity of noodles and sauce needed to make a lasagna.
 *
 * @param {string[]} layers - Layers of ingredients to be added to the lasagna.
 *
 * @param {Quantities} quantities - The default object to return, which holds
 * the quantities of noodles and sauce.
 *
 * @returns {Quantities} Quantity of noodles and sauce needed to make your meal.
 *
 * @description This function calculates the total quantity of noodles and sauce
 * needed to make a lasagna. It takes an array of `layers` as input, where each
 * layer is a string that represents an ingredient. The function returns an
 * object with the total weight of noodles and volume of sauce required.
 */
export function quantities(layers, quantities={ noodles: 0, sauce: 0 }) {
  for (const ingredient of layers) if (ingredient in QUANTITIES)
    quantities[ingredient] += QUANTITIES[ingredient];
  return quantities; }

/**
 * Adds the last ingredient from your friend's lasagna to your own lasagna.
 *
 * @param {string[]} ingredients - Your friend's lasagna w/ a secret ingredient.
 *
 * @param {string[]} layers - Layers of ingredients to prepare your lasagna.
 *
 * @returns {void} This function doesn't return anything because it modifies
 * the `layers` array directly by appending the secret ingredient from your
 * friend's lasagna. The `void` keyword is used to indicate that the function
 * does not have a return statement.
 *
 * @description This function adds the last ingredient from your friend's
 * lasagna (the secret ingredient) to your own lasagna. It takes 2 arrays:
 * `ingredients`, which represents your friend's lasagna ingredients; and
 * `layers`, which represents your own lasagna layers.
 * The secret ingredient is assumed to be the last item in `ingredients`.
 */
export const addSecretIngredient = (ingredients, layers) => void
  layers.push( ingredients[ingredients.length - 1] );

/**
 * Scales the ingredients of a recipe based on the desired number of portions.
 *
 * @param {Object<string, number>} recipe - The original recipe object, where
 * each key is an ingredient and its value is the amount needed for 2 portions.
 *
 * @param {number} portion - The desired number of portions. Defaults to 2.
 *
 * @returns {Object<string, number>} A new recipe object with the ingredients
 * scaled for the desired number of portions.
 *
 * @description The original recipe is designed for 2 portions.
 * This function returns a new recipe object with the ingredients scaled for
 * the desired number of portions, without modifying the original recipe object.
 */
export function scaleRecipe(recipe, portion=2) {
  const scale = portion / 2, scaledRecipe = { ...recipe };
  for (const ingredient in recipe) scaledRecipe[ingredient] *= scale;
  return scaledRecipe; }
