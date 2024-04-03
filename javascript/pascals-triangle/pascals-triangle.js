// @ts-check

const makeRow = (_, idx=0) => /** @type {number[]} */ (Array(idx + 1).fill(1));

export const rows = (rowNum=0) => {
  const pascal = Object.freeze( Array.from({ length: rowNum }, makeRow) );

  for (var prev = pascal[1], i = 1; ++i < rowNum; prev = row)
    for (var row = pascal[i], j = 0; ++j < i; row[j] = prev[j] + prev[j - 1]);

  return console.table(pascal), pascal; }
