// @ts-check

////////////////////////////////////////////////////////////////////////////

/** @param {string | number} chars */
const pad = (chars, len=3) => ('' + chars).padStart(len);

////////////////////////////////////////////////////////////////////////////

/** t = Team, m = Matches, w = Wins, d = Draws, l = Losses, p = Points */
export class StatClass {
  m = 0; w = 0; d = 0; l = 0; p = 0;

  constructor(name='') {
    this.t = name;
  }

  reset() {
    this.w = this.d = this.l = this.p = 0;
    return this;
  }

  total() {
    return this.p = 3 * this.w + this.d;
  }

  toString() {
    const { t, m, w, d, l, p } = this;

    return t.padEnd(SPACES) +
           `|${pad(m)} |${pad(w)} |${pad(d)} |${pad(l)} |${pad(p)}`;
  }
}

////////////////////////////////////////////////////////////////////////////

/** @constructor Classic Constructor Function */
export function StatFunc(name='') {
  this.t = name, this.m = 0, this.w = 0, this.d = 0, this.l = 0, this.p = 0;
}

const
  reset = StatFunc.prototype.reset = StatClass.prototype.reset,
  total = StatFunc.prototype.total = StatClass.prototype.total,
  toString = StatFunc.prototype.toString = StatClass.prototype.toString;

////////////////////////////////////////////////////////////////////////////

/** Object Creator Function */
export function StatObj(name='') {
  return { t: name, m: 0, w: 0, d: 0, l: 0, p: 0, reset, total, toString };
}

////////////////////////////////////////////////////////////////////////////

/** MP = Matches, W = Wins, D = Draws, L = Losses, P = Points */
export const
  HEADER = 'Team                           | MP |  W |  D |  L |  P',

  SPACES = HEADER.indexOf('|'),
  
  RESULTS = Object.freeze(/** @type {const} */ ([ 'loss', 'draw', 'win' ])),

  STATS = Object.freeze(
    /** @type {typeof StatClass[]} */ ([ StatClass, StatFunc, StatObj ]));

////////////////////////////////////////////////////////////////////////////
