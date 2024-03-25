// @ts-check

export class BankAccount {
  #money = NaN;

  /** @throws {ValueError} if bank account is already active */
  open(val=this.#money) {
    return val || val == 0 ? this.#err() : this.#money = 0, this; }

  /** @throws {ValueError} if bank account is closed */
  close() { return this.balance, this.#money = NaN, this; }

  /**
   * @param {number} val
   * @throws {ValueError} if bank account is closed or `val` is negative
   */
  deposit(val) { return this.balance, val < 0 ? this.#err() : this.#op(val); }

  /**
   * @param {number} val
   * @throws {ValueError} if bank account is closed or not enough funds
   */
  withdraw(val) {
    return val < 0 || this.balance < val ? this.#err() : this.#op(-val); }

  /** @param {number} val */
  #op(val) { return isFinite(val) && (this.#money += +val), this; }

  /** @throws {ValueError} exception related to bank account operations */
  #err() { throw new ValueError; }

  /** @throws {ValueError} if bank account is closed */
  get balance() {
    if (this.#money || this.#money == 0) return this.#money
    throw new ValueError; } }

export class ValueError extends Error {
  constructor() { super('Bank account error'); } }
