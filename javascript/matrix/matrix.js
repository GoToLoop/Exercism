// @ts-check

export class Matrix {
  constructor(s='') {
    this.r = s.split('\n').map(row => row.split(' ').map(Number));
    this.c = this.r[0].map((_, colIdx) => this.r.map(row => row[colIdx])); }

  get rows() { return this.r; } get columns() { return this.c; } }
