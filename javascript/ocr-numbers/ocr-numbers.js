// @ts-check

const { freeze: icy } = Object, { from } = Array, SPC = / +$|\n/g,

      clean = (s='') => s.replace(SPC, ''), digits = (s='') => s.length/12 | 0,
      digitIdx = (num='', idx=0) => ~(idx = DIGITS.indexOf(num)) ? idx : '?',

      multiScan = (ocr='') => icy(ocr.split(REGEX).map((s, _, __, c=clean(s)) =>
        icy(from({ length: digits(s) }, (_, i) => scanDigit(i, c))))),

      REGEX = /(?<=\n\s{3,})\n/, OCR = ' _     _  _     _  _  _  _  _ '
        + '| |  | _| _||_||_ |_   ||_||_|' + '|_|  ||_  _|  | _||_|  ||_| _|',

      DIGITS = icy(from({ length: 10 }, (_, i) => scanDigit(i)));

function scanDigit(pos=0, ocr=OCR) {
  const len = ocr.length, cols = len/3 | 0, idx = pos*3;
  for (var digit='', i = idx; i < len; i += cols) digit += ocr.substr(i, 3);
  return digit; }

export function convert(ocr=OCR, blocks = multiScan(ocr)) {
  for (var val = '', i = 0; i < blocks.length; val += ',')
    for (const digit of blocks[i++]) val += digitIdx(digit);
  return console.log(ocr + '\n', val = val.slice(0, -1)), val; }
