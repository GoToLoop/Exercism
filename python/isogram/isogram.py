def is_isogram(s: str):
    return len(t := tuple(filter( str.isalpha, s.lower() ))) == len(set(t))
