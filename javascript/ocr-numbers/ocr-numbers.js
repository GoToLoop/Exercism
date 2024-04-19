// @ts-check

const { freeze: icy } = Object, { from } = Array, SPC = / +$/,

      cleanup = (ocr='') => ocr.replace(SPC, '')['replaceAll']('\n', '') + '',
      numDigits = ({ length=0 }) => length / 12 | 0,
      digitIdx = (num='', idx=0) => ~(idx = DIGITS.indexOf(num)) ? idx : '?',

      multiScan = (ocr='') => icy(ocr.split(REGEX).map(str => icy(from(
        { length: numDigits(str) }, (_, i) => scanDigit(i, cleanup(str)))))),

      REGEX = /(?<=\n\s{3,})\n/, OCR = ' _     _  _     _  _  _  _  _ '
        + '| |  | _| _||_||_ |_   ||_||_|' + '|_|  ||_  _|  | _||_|  ||_| _|',

      DIGITS = icy(from({ length: 10 }, (_, i) => scanDigit(i)));

function scanDigit(pos=0, ocr=OCR) {
  const len = ocr.length, cols = len / 3 | 0, idx = pos * 3;
  for (var num='', i = idx; i < len; i += cols) num += ocr.substr(i, 3);
  return num; }

export function convert(ocr=OCR, lines = multiScan(ocr)) {
  for (var val = '', i = 0; i < lines.length; val += ',')
    for (const digit of lines[i++]) val += digitIdx(digit);
  return val.slice(0, -1); }
