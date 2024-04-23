// @ts-check

/** @typedef {Object<string, string>} Macro */

var last = 0, penult = 0;

export class Forth {
  stack = /** @type {number[]} */([]); macros = /** @type {Macro} */({});

  evaluate(exp='', {stack, macros}=this) {
    if ((exp = exp.toLowerCase())[0] == ':') return this.#define(exp);

    for (const cmd of exp.split(' '))
      if (isFinite(+cmd)) stack.push(+cmd | 0);
      else if (macros[cmd]) this.evaluate(macros[cmd]);
      else if (!this[cmd]?.()) throw Error('Unknown command'); return this; }

  #define(exp='', {macros}=this, idx=0) {
    const [ cmd, ...args ] = exp.slice(2, -2).split(' ');
    if (isFinite(+cmd)) throw Error('Invalid definition');
    for (const arg of args) args[idx++] = macros[arg] || arg;
    return macros[cmd] = args.join(' '), this; }

  #empty(len=this.stack.length) { if (len < 2) throw Error('Stack empty'); }

  #top({stack}=this, len=stack.length) {
    if (len) return last = stack[len-1], this; throw Error('Stack empty'); }

  #top2({stack}=this, len=stack.length) {
    return this.#empty(), last = stack[len-1], penult = stack[len-2], this; }

  #pop2({stack: s}=this) {
    return this.#empty(), last = s.pop() || 0, penult = s.pop() || 0, this; }

  over() { return this.#top2().stack.push(penult), this; }
  dup() { return this.#top().stack.push(last), this; }
  drop() { return this.#top().stack.pop(), this; }

  swap({stack}=this, len=stack.length) {
    return this.#top2().stack[len-1] = penult, stack[len-2] = last, this; }

  '+'() { return this.#pop2().stack.push(penult + last), this; }
  '-'() { return this.#pop2().stack.push(penult - last), this; }
  '*'() { return this.#pop2().stack.push(penult * last), this; }

  '/'() {
    if (this.#pop2(), last) return this.stack.push(penult / last | 0), this;
    throw Error('Division by zero'); } }
