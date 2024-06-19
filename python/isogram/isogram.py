is_isogram = lambda s: len(t:=(*filter(str.isalpha, s.lower()),)) == len(set(t))
