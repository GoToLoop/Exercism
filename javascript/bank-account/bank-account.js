// @ts-check

/** Custom error class for bank account-related exceptions. */
export class ValueError extends Error {
  constructor() { super('Bank account error'); } }

/** Represents a bank account. */
export class BankAccount {
  /** The amount of money in the bank account. */
  #money = NaN;

  /**
   * Getter for the balance of the bank account stored in `#money`.
   * @throws {ValueError} if the bank account is closed.
   * @returns {number} The account balance.
   */
  get balance() { return isFinite(this.#money) ? this.#money : +this.#err(); }

  /**
   * Activates the bank account.
   * @throws {ValueError} if the bank account is already active.
   * @returns {this} A ready-to-use bank account.
   */
  open() { return isFinite(this.#money) ? this.#err() : this.#money = 0, this; }

  /**
   * Closes the bank account.
   * @throws {ValueError} if the bank account has already been closed.
   * @returns {this} A deactivated bank account.
   */
  close() { return this.balance, this.#money = NaN, this; }

  /**
   * Deposits funds into the bank account.
   * @param {number} val The amount to deposit.
   * @throws {ValueError} if the bank account is closed or `val` is negative.
   * @returns {this} The updated bank account.
   */
  deposit(val) { return this.balance, val < 0 ? this.#err() : this.#op(val); }

  /**
   * Withdraws funds from the bank account.
   * @param {number} val The amount to withdraw.
   * @throws {ValueError} if the bank account is closed or `val` is negative
   * or not enough funds.
   * @returns {this} The updated bank account.
   */
  withdraw(val) {
    return this.balance < val || val < 0 ? this.#err() : this.#op(-val); }

  /**
   * Performs an account operation (deposit or withdrawal).
   * @param {number} val The value to add or subtract.
   * @returns {this} The updated bank account.
   */
  #op(val) { return isFinite(val) && (this.#money += val), this; }

  /**
   * Throws a `ValueError` related to bank account operations.
   * @throws {ValueError} Always throws "Bank account error"!
   * @returns {this} It never returns!
   */
  #err() { return eval('throw new ValueError;'), this } }
