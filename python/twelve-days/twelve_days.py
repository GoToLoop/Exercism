def recite(lo: int, hi: int) -> list[str]:
    """Recite the verses of the song "12 Days of Christmas" for a given range.

    This function generates the lyrics for the song based on the specified
    range of days. Each verse corresponds to a day, and the verses are recited
    in reverse order for each day.

    :param int lo: Starting day (1 to 12) of the song verse.
    :param int hi: Ending day (1 to 12) of the song verse.

    :return list[str]: A list containing the string of the joined verses for
    the given range of days when `lo` and `hi` are the same. Otherwise, it
    returns a list of strings, each representing the joined verses for each
    day in the specified range.
    """
    # if lo * hi == 0 or lo | hi < 0: return ['']

    if lo != hi: return [ recite(i, i)[0] for i in range(lo, hi + 1) ]

    verses = [ INTRO % DAYS[lo - 1], *GIFTS[lo - 1::-1] ]

    if lo != 1: verses[-1] = AND + verses[-1]

    return print(verses := [ ''.join(verses) ]) or verses


INTRO = 'On the %s day of Christmas my true love gave to me: '; AND = 'and '

GIFTS = ('a Partridge in a Pear Tree.', 'two Turtle Doves, ',
'three French Hens, ', 'four Calling Birds, ', 'five Gold Rings, ',
'six Geese-a-Laying, ', 'seven Swans-a-Swimming, ', 'eight Maids-a-Milking, ',
'nine Ladies Dancing, ', 'ten Lords-a-Leaping, ', 'eleven Pipers Piping, ',
'twelve Drummers Drumming, '); DAYS = ('first', 'second', 'third', 'fourth',
'fifth', 'sixth', 'seventh', 'eighth', 'ninth', 'tenth', 'eleventh', 'twelfth')
