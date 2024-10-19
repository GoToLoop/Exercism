def is_paired(brackets: str, s: list[str]=[], A='{[(', B='}])'):
    for c in print(brackets) or s.clear() or brackets:
        if c in A: s += c
        elif c in B and (not s or B.index(c) != A.index(s.pop())): return False
    return not s
