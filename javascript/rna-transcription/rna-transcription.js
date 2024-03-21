// @ts-check

/** @typedef {keyof typeof DNA_TO_RNA} RNA */

const DNA_TO_RNA = Object.freeze({ G: 'C', C: 'G', T: 'A', A: 'U' });

/** @param {RNA} nucleotide */
const transcriber = nucleotide => DNA_TO_RNA[nucleotide];

/** @param {string} dna */
export function toRna(dna) {
  const nucleotides = /** @type {RNA[]} */ ( dna.split('') );
  return nucleotides.map(transcriber).join(''); }
