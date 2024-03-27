// @ts-check

const SHARPS = 'A, A#, B, C, C#, D, D#, E, F, F#, G, G#'.split(', '),
      FLATS  = 'A, Bb, B, C, Db, D, Eb, E, F, Gb, G, Ab'.split(', '),
      NOTES = 'A, B, C, D, E, F#, G, a, b, c#, d#, e, f#, g#'.split(', '),
      INTERS = 'mMA';

/**
 * Represents a musical scale.
 * @prop {string} note - the initial tonic of the scale
 * @prop {string} up - the initial tonic in uppercase
 * @prop {number} idx - the index of the tonic in the notes array
 * @prop {string[]} notes - if `note` in NOTES it's SHARPS, otherwise FLATS
 */
export class Scale {
  constructor(n='') {
    const u = this.up = (this.note = n)[0].toUpperCase() + (n[1] || '');
    this.idx = (this.notes = NOTES.includes(n) && SHARPS || FLATS).indexOf(u); }

  chromatic() {
    return this.notes.slice(this.idx).concat(this.notes.slice(0, this.idx)); }

  interval(intervals='', notes=this.notes, len=notes.length, idx=this.idx) {
    return [ this.up, ...Array.prototype.map.call(intervals,
      step => notes[ idx = (idx + 1 + INTERS.indexOf(step)) % len ]) ]; } }
