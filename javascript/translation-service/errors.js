export class NotAvailable extends Error {
  /** @param {string} text */
  constructor(text) {
    super(`The requested text "${text}" has not been translated yet.`);
  }
}

export class AbusiveClientError extends Error {
  constructor() {
    super(`Your client has been rejected because of abusive behaviour.

naDevvo’ yIghoS!`);
  }
}

export class Untranslatable extends Error {
  constructor() {
    super('jIyajbe’');
  }
}
