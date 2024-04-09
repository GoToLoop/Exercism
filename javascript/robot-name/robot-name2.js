// @ts-check

export class Robot {
  static #allNames = this.#nameGen(); static #names = this.#allNames.slice();

  static #nameGen(all=/** @type {string[]} */([])) {
    for (var chr = String.fromCharCode, ab = '', k = 0, i = 65; i < 91; )
      for (var a = chr(i++), j = 65; ab = a + chr(j), j++ < 91; k = 0) {
        while (k < 1000) all.push(ab + (k++ + '').padStart(3, '0')); }

    return Object.freeze(all); }

  static releaseNames() { this.#names = this.#allNames.slice(); }

  reset(name='', names=Robot.#names, len=names.length) {
    name = this._name = names.splice(Math.random() * len, 1)[0]; return name }

  _name = this.reset(); get name() { return this._name; } }
