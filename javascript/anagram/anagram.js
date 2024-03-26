// @ts-check

/**
 * Finds anagrams of a given `word` within an array of `words`.
 * @param {string} word the target `word` to find anagrams for
 * @param {string[]} words the array of anagram candidates for input `word`
 * @returns {string[]} an array of anagrams found in the `words` array
 */
export const findAnagrams = (word, words, sorted = sortWord(word)) =>
  words.filter(w => sortWord(w) == sorted && noCase.compare(w, word));

/**
 * Sorts the characters of a given word in alphabetical ascending order.
 * @param {string} w the word to sort and convert to lowercase letters
 * @returns {string} the sorted lowercase version of the input word
 */
export const sortWord = w => [ ...w.toLowerCase() ].sort().join('');

/** This is a case-insensitive string comparison instance of Collator. */
const noCase = Intl.Collator(void 0, { sensitivity: 'base' });
