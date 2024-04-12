// @ts-check

/** @typedef {typeof SCHEDULES[number]} Schedule */
/** @typedef {typeof WEEK_DAYS[number]} WeekDay */

const SCHEDULES = Object.freeze(/** @type {const} */
  ([ 'first', 'second', 'third', 'fourth', 'last', 'teenth' ])),

      WEEK_DAYS = Object.freeze(/** @type {const} */([ 'Sunday', 'Monday',
  'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday' ]));

/**
 * Finds date of a meetup given year, month and recurring weekday schedule.
 *
 * @param {number} year - meetup's year
 * @param {number} month - meetup's month index (1-based) value
 * @param {Schedule} schedule - meetup's schedule description
 * @param {WeekDay} week - meetup's target day of the week
 *
 * @returns {Date} the date within given `year` & `month` that matches both
 * `schedule` & day of the `week`
 */
export function meetup(year, month, schedule, week) {
  const date = setDay(new Date(year, month - 1), WEEK_DAYS.indexOf(week));

  if (schedule == 'teenth') return teenth(date), date;
  if (schedule == 'last') return last(date), date;

  return occurrence(date, schedule), date; }

/**
 * Adjusts the provided date to the next occurrence of the specified weekday.
 *
 * @param {Date} date - the date to be modified and returned
 * @param {number} week - 0-based indexed target day of the week (0-6)
 *
 * @returns {Date} the first upcoming date from the provided date that matches
 * the target day of the week
 */
export function setDay(date, week) {
  return date.setDate(date.getDate() + (week - date.getDay() + 7) % 7), date; }

/** @param {Date} date */
function teenth(date) {
  for (var day = 0; (day = date.getDate()) < 13; date.setDate(day + 7)); }

/** @param {Date} date */
function last(date) {
  const days = new Date(date.getFullYear(), date.getMonth() + 1, 0).getDate();
  for (var day = 0; (day = date.getDate() + 7) <= days; date.setDate(day)); }

/** @param {Date} date @param {Schedule} schedule*/
function occurrence(date, schedule) {
  date.setDate(date.getDate() + 7 * SCHEDULES.indexOf(schedule)); }
