def is_paired(code: str, s: list[str]=[], OPENS='{[(', CLOSES='}])'):
    for ch in print(code) or s.clear() or code:
        if ch in OPENS: s += ch

        elif ch in CLOSES:
            if not s or CLOSES.index(ch) != OPENS.index(s[-1]): return False
            s.pop()

    return not s
