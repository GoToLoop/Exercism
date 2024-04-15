// @ts-check

const matrix = (length=0) => Object.freeze(Array.from({ length }, () =>
  /** @type {number[]} */(Array(+length)).fill(0)));

export function spiralMatrix(l=0, mat=matrix(l=Math.abs(l|0)), row=mat[0]) {
  var startRow=0, endRow=l, startCol=0, endCol=l, ll=l*l, num=1, i=0;

  while (num <= ll) {
    // Fill the top row from left to right:
    for (i = startCol, row = mat[startRow++]; i < endCol; row[i++] = num++);
    // `startRow++` makes the 2nd row the current top row!

    // Fill the rightmost column from top to bottom:
    for (i = startRow, --endCol; i < endRow; mat[i++][endCol] = num++);
    // `--endCol` makes the penultimate column the current last column!

    // Fill the bottom row from right to left:
    for (i = endCol, row = mat[--endRow]; --i >= startCol; row[i] = num++);
    // `--endRow` makes the penultimate row the current bottom row!

    // Fill the leftmost column from bottom to top:
    for (i = endRow; --i >= startRow; mat[i][startCol] = num++); ++startCol; }
    // `++startCol` makes the 2nd column the current 1st column!

  return mat; }
