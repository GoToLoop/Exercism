// @ts-check

/**
 * @param {Object<number, string[]>} oldModel
 * @param {Object<string, number>} newNodel
 */
export function transform(oldModel, newNodel={}) {
  for (const score in oldModel) for (const letter of oldModel[score])
    newNodel[letter.toLowerCase()] = +score;
  return newNodel; };
