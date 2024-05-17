/// <reference path="global.d.ts" />
// @ts-check

/**
 * Creates a new visitor.
 *
 * @param {string} name the name of the visitor
 * @param {number} age the age of the visitor
 * @param {string} ticketId the ticket ID for the visitor
 * @returns {Visitor} the visitor that was created
 */
export const createVisitor = (name, age, ticketId) => ({ name, age, ticketId });

/**
 * Revokes a ticket for a visitor.
 *
 * @param {Visitor} visitor the visitor with an active ticket
 * @returns {Visitor} the visitor without a ticket
 */
export const revokeTicket = visitor => (visitor.ticketId = null, visitor);

/**
 * Determines the status a ticket has in the ticket tracking object, either
 * the owner of the `ticketId` if found or some error message.
 *
 * @param {Object<string, string?>} tickets object tracking all tickets
 * @param {string} ticketId the ID of the ticket to check
 * @returns {string} either the corresponding owner of the `ticketId` or
 * its id status: 'not sold' when `null` or 'unknown' otherwise
 */
export function ticketStatus(tickets, ticketId, _owner=tickets[ticketId]) {
  if (_owner) return 'sold to ' + _owner;
  return _owner === null ? 'not sold' : 'unknown ticket id'; }

/**
 * Determines the status a ticket has in the ticket tracking object and
 * returns a simplified status message.
 *
 * @param {Object<string, string?>} tickets object tracking all tickets
 * @param {string} ticketId the ID of the ticket to check
 * @returns {string} ticket status or its owner
 */
export const simpleTicketStatus = (tickets, ticketId) =>
  tickets[ticketId] ?? 'invalid ticket !!!';

/**
 * Determines the version of the GTC that was signed by the visitor.
 *
 * @param {VisitorWithGtc} visitor the visitor who agreed to the GTC
 * @returns {string=} the version of the GTC that was agreed to if available, 
 * otherwise returns undefined, indicating the visitor needs to sign for GTC
 */
export const gtcVersion = visitor => visitor.gtc?.version; 
