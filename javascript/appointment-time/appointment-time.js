// @ts-check

/**
 * @typedef {Object} Appointment The date & time parts of an appointment with
 *                               type Date compatible range values
 *
 * @property {number} year The year value (4-digit)
 * @property {number} month The month value (0–11)
 * @property {number} date The date value (1–31)
 * @property {number} hour The hour value (0–23)
 * @property {number} minute The minute value (0–59)
 */

/**
 * Create an appointment
 *
 * @param {number} days number of days till the appointment
 * @param {number=} now ms since the epoch 1970 (defaults to current elapsed ms)
 *
 * @returns {Date} the appointment date
 */
export function createAppointment(days, now=Date.now()) {
  const d = new Date(now); d.setDate(d.getDate() + days); return d; }

/**
 * Generate the appointment timestamp from a date
 *
 * @param {Date} date the appointment date
 *
 * @returns {string} timestamp
 */
export function getAppointmentTimestamp(date) { return date.toISOString(); }

/**
 * Get details of an appointment from a timestamp
 *
 * @param {string} stamp (ISO 8601 timestamp): 'YYYY-MM-DDTHH:mm:ss.mssZ'
 *
 * @returns {Appointment} the appointment's date & time
 */
export function getAppointmentDetails(stamp) { const d = new Date(stamp);
  return { year: d.getFullYear(), month: d.getMonth(), date: d.getDate(),
           hour: d.getHours(), minute: d.getMinutes() }; }

/**
 * Update an appointment timestamp with given options
 *
 * @param {string} stamp (ISO 8601 timestamp)
 * @param {Partial<Appointment>} opts appointment properties to be overwritten
 *
 * @returns {Appointment} the appointment's date & time
 */
export function updateAppointment(stamp, opts) {
  const a = getAppointmentDetails(stamp); for (const p in opts) a[p] = opts[p];
  return getAppointmentDetails(getAppointmentTimestamp(toDate(a))); }

/**
 * Convert an appointment to a date object
 *
 * @param {Appointment} a the appointment details (date + time)
 *
 * @returns {Date} the appointment date
 */
export const toDate = a => new Date(a.year, a.month, a.date, a.hour, a.minute);

/**
 * Get available time in seconds (rounded) between two appointments
 *
 * @param {string} stampA (ISO 8601 timestamp)
 * @param {string} stampB (ISO 8601 timestamp)
 *
 * @returns {number} amount of seconds (rounded)
 */
export function timeBetween(stampA, stampB) {
  return Math.round( (+new Date(stampB) - +new Date(stampA)) / 1000 ); }

/**
 * Check if appointment is still in the future compared to present stamp
 *
 * @param {string} schedule future appointment (ISO 8601 timestamp)
 * @param {string} now present stamp representation (ISO 8601 timestamp)
 * 
 * @returns {boolean} true if appointment haven't happened yet
 */
export const isValid = (schedule, now) => new Date(schedule) > new Date(now);
