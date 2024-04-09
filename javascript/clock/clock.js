// @ts-check

const wrap = (v=0, m=1440) => (~~v % (m |= 0) + m) % m,
      pad = v => ('' + v).padStart(2, '0');

export class Clock {
  constructor(h=0, m=0) { this.m = wrap(~~h*60 + ~~m); }

  toString({m}=this) { return pad(wrap(m/60, 24)) + ':' + pad(wrap(m, 60)); }

  plus(m=0) { return this.m = wrap(this.m + ~~m), this; }

  minus(m=0) { return this.plus(-m); }

  equals({m}=this) { return m == this.m; } }
