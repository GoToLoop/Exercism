/**
 * @typedef {Object} Visitor - Visitor with an active ticket.
 * @property {string} name - The name of the visitor.
 * @property {number} age - The age of the visitor.
 * @property {string?} ticketId - The ticket ID of the visitor.
 */

/**
 * @typedef {Object} Gtc - (General Terms & Conditions)
 * @property {string} version - The version of GTC.
 */

/**
 * @typedef {Object} VisitorWithGtc - Visitor who agreed to the GTC.
 * @property {Visitor} visitor - Visitor with an active ticket.
 * @property {Gtc} [gtc] - The agreed version of GTC.
 */
