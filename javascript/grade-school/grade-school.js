// @ts-check

export class GradeSchool {
  #school = /** @type {Object<number, string[]>} */({});

  add(student='', grade=NaN) {
    this.#seekAndDestroy(student);
    const names = this.#school[grade] || (this.#school[grade] = []);
    return names.push(student), names.sort(), this; }

  roster() { return JSON.parse(JSON.stringify(this.#school)); }

  grade(grade=NaN) { return this.#school[grade]?.slice() || []; }

  #seekAndDestroy(student='') {
    for (const grade in this.#school) {
      const names = this.#school[grade], idx = names.indexOf(student);
      if (~idx) return names.splice(idx, 1); } } }
