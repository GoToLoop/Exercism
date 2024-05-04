// @ts-check

const calcLen = (letter=' ') => letter.toUpperCase().charCodeAt(0) - 66,
      padChar = (ch=' ', len=1, col=0) => (' '.repeat(col) + ch).padEnd(len),
      reverse = (s='') => Array.prototype.toReversed.call(s).join('') + '';

export function rows(ch='A', rows=calcLen(ch), cols=rows + 1, row='', i=0) {
  if (rows < 0) return [ ch ];

  const diamond = [ (row = ' '.repeat(cols)) + 'A' + row ],
        rowMid = ch + ' '.repeat((cols << 1) - 1) + ch;

  while (i < rows) {
    row = padChar(String.fromCharCode(i + 66), cols, i++);
    diamond.push(reverse(row) + ' ' + row) };

  diamond.push(rowMid, ...diamond.toReversed());
  console.table(diamond); return diamond; }
