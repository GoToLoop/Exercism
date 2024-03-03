def is_pangram(s: str): return len(set(filter( str.isalpha, s.lower() ))) == 26
