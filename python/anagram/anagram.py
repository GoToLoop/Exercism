def sort(w: str): return ''.join(sorted(w))

def find_anagrams(word: str, words: list[str]):
    s = sort(word := word.lower())
    return [*filter(lambda w: s == sort(w := w.lower()) and word != w, words)]
