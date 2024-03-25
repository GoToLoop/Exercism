// @ts-check

/** @constructor */
export function Squares(n=0) {
  this.sumOfSquares = (this.squareOfSum = n * (n + 1) >> 1) * (2*n + 1) / 3;
  this.difference = (this.squareOfSum **= 2) - this.sumOfSquares; }
