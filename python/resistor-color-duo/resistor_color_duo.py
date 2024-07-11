INKS = *'black brown red orange yellow green blue violet grey white'.split(),

def value(c: list[str]): return 10 * INKS.index(c[0]) + INKS.index(c[1])

# def value(c: list[str]): return int(f"{INKS.index(c[0])}{INKS.index(c[1])}")
