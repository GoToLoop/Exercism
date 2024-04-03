// @ts-check

export const truncate = (txt='') => [ ...txt ].slice(0, 5).join('');

/*
const MAX = 5, isLeadSurrogate = (txt='', idx=0, code=txt.charCodeAt(idx)) =>
  code >= 0xD800 && code <= 0xDBFF;

export function truncate(txt='', i=0, idx=0) {
  while (i++ < MAX) idx += +isLeadSurrogate(txt, idx) + 1;
  return txt.slice(0, idx); }
 */
