const DAILY_WORK_HOURS = 8, MONTHLY_BILLABLE_DAYS = 22;

/**
 * Calculates the day rate pay given a rate per hour.
 *
 * @param {number} ratePerHour the rate pay per hour
 * @returns {number} the rate pay per day
 */
export function dayRate(ratePerHour) {
  return DAILY_WORK_HOURS * ratePerHour;
}

/**
 * Calculates the number of workdays given a fixed budget rounded down.
 *
 * @param {number} budget the total budget
 * @param {number} ratePerHour the rate pay per hour
 * @returns {number} the number of working days rounded down
 */
export function daysInBudget(budget, ratePerHour) {
  return budget / dayRate(ratePerHour) | 0;
}

/**
 * Calculates the discounted cost for large projects, rounding up final price.
 * Discount is applied to full months only. Remaining days are billed fully.
 *
 * @param {number} ratePerHour the rate pay per hour
 * @param {number} numDays the number of days the project spans
 * @param {number} discount project's discount rate (20% is represented as 0.2)
 * @returns {number} the deducted total price rounded up
 */
export function priceWithMonthlyDiscount(ratePerHour, numDays, discount) {
  const
    remainingDays = numDays % MONTHLY_BILLABLE_DAYS,
    discountDays  = numDays - remainingDays,
    billableDays  = (1 - discount) * discountDays + remainingDays;

  return Math.ceil(dayRate(ratePerHour) * billableDays);
}
