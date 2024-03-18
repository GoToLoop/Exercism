// @ts-check

export class ArgumentError extends Error {}

export class OverheatingError extends Error {
  constructor(temperature=0) {
    super(`The temperature is ${temperature} ! Overheating !`);
    this.temperature = temperature; }}

/**
 * Check if the humidity level is not too high.
 *
 * @param {number} humidityPercentage the humidity % level of the room
 *
 * @throws {Error}
 */
export function checkHumidityLevel(humidityPercentage) {
  if (humidityPercentage > 70) throw Error('Too much humidity in the room!'); }

/**
 * Check if the temperature is not too high.
 *
 * @param {number | null} t machines' temperature
 *
 * @throws {ArgumentError | OverheatingError}
 */
export function reportOverheating(t) {
  if (t == null) throw new ArgumentError('Sensor is broken!');
  else if (t > 500) throw new OverheatingError(t); }

/**
 *  Triggers the needed action depending on the result of the machine check.
 *
 * @param {{
 *  check: function, // checks the temperature of the machine
 *  alertDeadSensor: function, // alerts a technician temp's sensor is dead
 *  alertOverheating: function, // turns on machine's overheating warning light
 *  shutdown: function // turns the machine off
 * }} actions
 *
 * @throws {ArgumentError|OverheatingError|Error}
 */
export function monitorTheMachine(actions) {
  try { actions.check(); } catch (err) {
    if (err instanceof ArgumentError) return actions.alertDeadSensor();
    if ('temperature' in err) return err.temperature > 600 ?
      actions.shutdown() : actions.alertOverheating();
    throw err; }}
