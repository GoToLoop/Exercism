// @ts-check

const { from, prototype: { reduce } } = Array, { freeze: icy } = Object,
      seriesLen = (len=0, span=0) => icy({ length: len + 1 - span }),
      productMap = (digits='') => +reduce.call(digits, productReduce),
      productReduce = (mult=0, digit='') => mult * +digit;

/** @throws {Error} */
export function largestProduct(inp='', span=0, len=inp.length) {
  if (span <= 0) throw Error('Span must be greater than zero');
  if (span > len) throw Error('Span must be smaller than string length');
  if (!isFinite(+inp)) throw Error('Digits input must only contain digits');

  const series = icy(from(seriesLen(len, span), (_, i) => inp.substr(i, span)));
  return Math.max(...series.map(productMap)); }
