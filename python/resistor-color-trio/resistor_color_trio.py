UNITS, BANDS = ("", "kilo", "mega", "giga"), ( "black", "brown", "red",
    "orange", "yellow", "green", "blue", "violet", "grey", "white" )

# Gets the corresponding BANDS indices of the first three colors in the input
# list as a tuple of resistance values:
def vals(colors: list[str]): return *(map(BANDS.index, colors[:3])),

def label(c: list[str]):
    """Calculates the resistance value based on the input color bands.

    :param list[str] c: a list of 3 color names representing resistor bands.
    :return str: resistance value with appropriate unit and 'ohms'.
    """

    v = str((n := vals(c))[1] + n[0] * ((z := not n[1]) and 1 or 10) or '')
    return v + (zz := z + n[2]) % 3 * '0' + ' ' + UNITS[zz // 3] + 'ohms'
