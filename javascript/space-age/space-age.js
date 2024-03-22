// @ts-check

/**
 * Calculates how old a person would be in a given planet from our star system.
 * @param {keyof PLANETS} p planet name in lower case
 * @param {number} a age in seconds
 * @returns {number} corresponding age in years on given planet
 */
export const age = (p, a) => +( a / DAY / YEAR / PLANETS[p] ).toFixed(2);

const DAY = 24 * 60 * 60, YEAR = 365.25, PLANETS = Object.freeze({ 
  jupiter: 11.862615, saturn: 29.447498, uranus: 84.016846, neptune: 164.79132,
  mercury: .2408467, venus: .61519726, earth: 1, mars: 1.8808158 });
