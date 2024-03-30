// @ts-check

const ord = (ch='') => ch.charCodeAt(0), chr = String.fromCharCode,
      isUpper = (ch='') => !(ord(ch) & 0x20), LEN = 26, A = ord('A'),
      ABC = Array.from({ length: LEN }, (_, i) => chr(i + A)).join('');

export function rotate(chars='', rot=0, cypher='') {
  for (const ch of chars) {
    const idx = ABC.indexOf(ch.toUpperCase()), char = ABC[(idx + rot) % LEN];
    cypher += ~idx ? isUpper(ch) && char || char.toLowerCase() : ch; }
  return cypher; }
