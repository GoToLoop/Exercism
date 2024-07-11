INKS = *'black brown red orange yellow green blue violet grey white'.split(),

TOLERANCES: dict[str, int | float] = { 'grey': .05, 'violet': .1,
    'blue': .25, 'green': .5, 'brown': 1, 'red': 2, 'gold': 5, 'silver': 10 }

UNITS = { 1_000_000_000: 'giga', 1_000_000: 'mega', 1_000: 'kilo' }

def inds(colors: list[str]): return *map(INKS.index, colors),

def resistor_label(colors: list[str]):
    """Calculates the resistance value based on the input color bands.

    :param list[str] colors: list of color names representing resistor bands
        - last color represents the tolerance value
        - penultimate color represents the magnitude multiplier
        - the 1st 2 or 3 colors represent the resistance values in ohms

    :return str: resistance value with appropriate unit+'ohms' & ±tolerance%
    """

    if len(colors) < 4: return "0 ohms"

    *n, mag, tol = inds(colors); tol = TOLERANCES[colors[-1]]; unit = ''

    val: int | float = int(''.join(map(str, n)) + '0' * mag)

    for u in UNITS:
        if val >= u: val /= u; unit = UNITS[u]; break

    return f'{int(val) if val == int(val) else val} {unit}ohms ±{tol}%'
