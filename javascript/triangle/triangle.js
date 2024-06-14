// @ts-check

export class Triangle {
  /** @param {number} a @param {number} b @param {number} c */
  constructor(a, b, c) {
    this.sides = Float32Array.of(a, b, c).map(Math.abs).sort();
    this.uniques = new Set(this.sides); }

  get isEquilateral() { return this.uniques.size == 1 && !!this.sides[0]; }

  get isIsosceles() {
    return this.isValid() && this.uniques.size && this.uniques.size < 3; }

  get isScalene() { return this.isValid() && this.uniques.size == 3; }

  isValid() { const [a, b, c] = this.sides; return a && b && c && a + b >= c; }}
