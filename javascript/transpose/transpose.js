// @ts-check

/** @param {*[]} arr */
const toLen = arr => arr.map(getLen), getLen = ({ length }) => length,
      maxLen = arr => Math.max(...toLen(arr));

/** @param {string[]} texts */
export function transpose(texts, transposed=[...''], len=maxLen(texts)) {
  for (var txt = '', ch = '', l = texts.length, j = 0, i = 0; i < len; ++i) {
    while (j < l) if ( ch = texts[j++][i] ) txt += ch;
                  else if (i < maxLen( texts.slice(j) )) txt += ' ';
    transposed.push(txt), txt = '', j = 0; }
  return transposed; }
