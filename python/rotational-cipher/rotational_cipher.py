rotate = lambda t='', r=0: ''.join(c.isalpha() and _abc(c, r) or c for c in t)

def _abc(c: str, rot: int, UP=ord('A'), LO=ord('a')):
    return chr((ord(c.upper()) - UP + rot) % 26 + (c.isupper() and UP or LO))
