// @ts-check

export class Robot {
  static #usedNames = /** @type {string[]} */([]);
  static releaseNames() { this.#usedNames.length = 0; }

  static rndAbc() { return String.fromCharCode(Math.random() * 26 + 65); }
  static rnd1K() { return String(Math.random() * 1000 | 0).padStart(3, '0'); }

  static nameGen() { return this.rndAbc() + this.rndAbc() + this.rnd1K(); }

  _name = this.reset(); get name() { return this._name; }

  reset(name='') {
    do name = Robot.nameGen(); while (Robot.#usedNames.includes(name));
    Robot.#usedNames.push(this._name = name); return name; } }
