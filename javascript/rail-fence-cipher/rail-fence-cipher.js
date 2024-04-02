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
