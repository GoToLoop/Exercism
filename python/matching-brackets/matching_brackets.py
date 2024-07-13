def is_paired(code: str, s='', OPENS='{[(', CLOSES='}])'):
    for ch in print(code) or code:
        if ch in OPENS: s += ch

        elif ch in CLOSES:
            if not s or CLOSES.index(ch) != OPENS.index(s[-1]): return False

            s = s[:-1]

    return not s
