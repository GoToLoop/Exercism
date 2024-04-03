// @ts-check

const strArr = (len=0) => /** @type {string[]} */ (Array(len).fill('')),
      numArr = (len=0) => new Uint8Array(len),
      bounce = (idx=0, tailIdx=0) => idx == 0 || idx == tailIdx ? -1 : 1;

export function encode(txt='', len=0, idx=0, dir=1, rails=strArr(len--)) {
  for (const ch of txt) rails[idx] += ch, idx += dir, dir *= bounce(idx, len);
  return rails.join(''); }

export function decode(enc='', len=0, dec='', lengs=numArr(len--)) {
  for (var idx = 0, dir = 1, i = 0, l = enc.length; i < l; ++i)
    ++lengs[idx], idx += dir, dir *= bounce(idx, len);

  idx = 0;
  const rails = Array.from(lengs, leng => enc.slice(idx, idx += leng));

  for (idx = 0, dir = 1, i = 0; i < l; ++i) {
    dec += rails[idx][0], rails[idx] = rails[idx].slice(1);
    idx += dir, dir *= bounce(idx, len); }

  return dec; }

// function convertToJS(n, x) {
//   return Math.round((2*n-2) * Math.abs((x/(2*n-2))
//     - Math.floor((x/(2*n-2)) + 0.5))); }

// fn rail_index(char_index: usize, rails: usize) ->
// usize { let repeat_position = 2 * rails + 2;
// (repeat_position * ((char_index + rails - 1) / repeat_position)).
// abs_diff(rails) }
