// @ts-check

/**
 * @callback Rule
 *
 * A function that represents a specific rule in the RULES object for
 * translating English words into Pig Latin. Each rule function inspects the
 * given word and checks if a certain condition, specific to the Pig Latin
 * language rules, is met. If the condition is met, the function applies a
 * transformation (as per Pig Latin conventions) to the word and returns the
 * transformed word. If the condition is not met, it returns an empty string,
 * indicating that this rule does not apply to the given word. Function
 * **translate()** is responsible for iterating over the RULES object in
 * order to transform words according to the specific rules of Pig Latin.
 *
 * @param {string} word - The English word to be checked and potentially
 * transformed into Pig Latin.
 *
 * @returns {string} - The transformed `word` in Pig Latin if the rule's
 * condition was met, or an empty string if the condition was not met,
 * indicating that this rule does not apply to the given `word`.
 */

export function translate(text='', words=text.split(' '), latin='') {
  for (var i = 0; i < words.length; ++i) for (const rule in RULES)
    if (latin = RULES[rule](words[i])) { words[i] = latin; break; }
  return console.info(text, latin = words.join(' ')), latin; }

const VOWEL_PATTERN = Object.freeze(/[aeiou]+|yt|xr/), AY = 'ay';

const RULES = Object.freeze(/** @type {Object<string, Rule>} */({
  /** Letter 'y' after consonant(s) is treated as a vowel. */
  rule4(w, i = w.search(VOWEL_PATTERN)) {
    if (~i && w[i] == 'y') return w.slice(i) + w.slice(0, i) + AY;
    return !~i && w.at(-1) == 'y' && 'y' + w.slice(0, -1) + AY || ''; },

  /** Matches 'qu' regardless if preceded or not by a consonant. */
  rule3(w, i = w.search(VOWEL_PATTERN)) {
    return ~i && w[i] == 'u' && w[i-1] == 'q' &&
      w.slice(++i) + w.slice(0, i) + AY || ''; },

  /** Word starts w/ 1 or more consonant sounds & has vowel letters too. */
  rule2(w, i = w.search(VOWEL_PATTERN)) {
    return i > 0 && w.slice(i) + w.slice(0, i) + AY || ''; },

  /** Word begins w/ a vowel sound, counting 'yt' & 'xr' as vowels too. */
  rule1(w, i = w.search(VOWEL_PATTERN)) { return !i && w + AY || ''; } }));
