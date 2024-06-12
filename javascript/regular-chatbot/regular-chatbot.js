// @ts-check

const MSGS = Object.freeze([ "Thanks! You can now download me to your phone.",
  "Oops, it seems like I can't reach out to " ]);

/**
 * Given a certain command, help the chatbot recognize whether the command is
 * valid or not, by checking if it starts with "Chatbot" (case insensitive).
 *
 * @param {string} command the text to check if it starts with a command
 * @returns {boolean} whether or not is the command valid
 */
export const isValidCommand = command => /^Chatbot/i.test(command);

/**
 * Given a certain message, help the chatbot get rid of all the emoji's
 * encryption through the message.
 *
 * @param {string} msg the message which may contain emoji codes
 * @returns {string} the message without the emojis encryption
 */
export const removeEmoji = msg => msg.replace(RegExp('emoji\\d+', 'g'), '');

/**
 * Given a certain phone number, help the chatbot recognize whether it is in
 * the correct format.
 *
 * @param {string} phone a string containing a phone number
 * @returns {string} the chatbot's response to the phone's validation
 */
export const checkPhoneNumber = phone =>
  /\(\+\d\d\)\s(\d{3}-){2}\d{3}/.test(phone) && MSGS[0] || MSGS[1] + phone;

/**
 * Given a certain response from the user, help the chatbot get only the URLs.
 *
 * @param {string} userInput a string containing simple URLs to capture
 * @returns {?string[]} all possible URLs the user may have answered
 */
export const getURL = userInput => userInput.match(/\w+(\.\w+)+/g);

/**
 * Greet the user using the full name data from the profile.
 *
 * @param {string} fullName in the form of "Surname, 1st Name"
 * @returns {string} greeting from the chatbot
 */
export const niceToMeetYou = fullName => "Nice to meet you, " +
  fullName.replace(/(\w*),\s(\w*)/, '$2 $1');
  // fullName.split(/,\s/).reverse().join(' ');
