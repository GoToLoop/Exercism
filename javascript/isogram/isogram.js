// @ts-check

export const isIsogram = (word='', s=[...word.toLowerCase()]) =>
  (s = s.filter(ch => ch >= 'a' && ch <= 'z')).length == new Set(s).size;
