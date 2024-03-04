// @ts-check

// Get those rates calculated!

const WORK_HOURS = 8, BILLABLE_DAYS = 22;

/**
 * Calculates the day rate given a rate per hour.
 *
 * @param {number} ratePerHour the rate per hour
 * @returns {number} the rate per day
 */
export function dayRate(ratePerHour) {
  return WORK_HOURS * ratePerHour;
}

/**
 * Calculates the number of workdays given a fixed budget rounded down.
 *
 * @param {number} budget the total budget
 * @param {number} ratePerHour the rate per hour
 * @returns {number} the number of days rounded down
 */
export function daysInBudget(budget, ratePerHour) {
  return budget / dayRate(ratePerHour) | 0;
}

/**
 * Calculates the discounted cost for large projects, rounding up final price.
 * Discount is applied to full months only. Remaining days are billed fully.
 *
 * @param {number} ratePerHour the rate per hour
 * @param {number} numDays the number of days the project spans
 * @param {number} discount project's discount rate (20% is represented as 0.2)
 * @returns {number} the deducted final price rounded up
 */
export function priceWithMonthlyDiscount(ratePerHour, numDays, discount) {
  const
    fullDays = numDays % BILLABLE_DAYS,
    discountDays = numDays - fullDays,
    billedDays = discountDays * (1 - discount) + fullDays;

  return Math.ceil(dayRate(ratePerHour) * billedDays);
}
