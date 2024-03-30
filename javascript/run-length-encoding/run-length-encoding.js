// @ts-check

export function encode(txt='', len = txt.length, enc='') {
  for ( var prev = txt[0], ch = '', seq = 1, i = 0; i++ < len; prev = ch )
    if ( (ch = txt[i]) == prev ) ++seq;
    else enc += seq == 1 && prev || seq + prev, seq = 1;
  return enc; }

export function decode(enc='', len = enc.length, dec='', digits = '') {
  for (const ch of enc)
    if ( ch != ' ' && isFinite(+(digits + ch)) ) digits += ch;
    else dec += ch.repeat(+digits || 1), digits = '';
  return dec; }
