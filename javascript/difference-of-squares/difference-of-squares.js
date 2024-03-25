// @ts-check

export class Squares {
  constructor(num=0) { this.n = num; }

  get squareOfSum() { return Squares.sum(this.n) ** 2; }
  get sumOfSquares() { return Squares.sum(this.n, 2); }
  get difference() { return this.squareOfSum - this.sumOfSquares; }

  /**
   * Calculate the sum of the first `n` natural numbers raised to the power `e`,
   * and keeping track of the current total sum in `t`.
   *
   * @param {number} n the number of terms to sum
   * @param {number} e the exponent to raise each term to
   * @param {number} t the running total of the sum
   *
   * @returns {number} the calculated sum
   */
  static sum(n=0, e=1, t=0) { return n ? Squares.sum(n-1, e, n**e + t) : t; } }
