"""a Partridge in a Pear Tree.
two Turtle Doves, 
three French Hens, 
four Calling Birds, 
five Gold Rings, 
six Geese-a-Laying, 
seven Swans-a-Swimming, 
eight Maids-a-Milking, 
nine Ladies Dancing, 
ten Lords-a-Leaping, 
eleven Pipers Piping, 
twelve Drummers Drumming, \rfirst
second
third
fourth
fifth
sixth
seventh
eighth
ninth
tenth
eleventh
twelfth"""

DOC = __doc__ or ''; G, D = ((*d.split('\n'),) for d in DOC.split('\r'))
I = 'On the %s day of Christmas my true love gave to me: '; A = 'and '

recite = lambda lo=1, hi=12: [ *map(versify, range(lo - 1, hi)) ]
"""Recite the verses of the song "12 Days of Christmas" for a given range.

This function generates the lyrics for the song based on the specified
range of days. Each verse corresponds to the gifts received on a day, and they
are recited in reverse order for each day. If `lo` and `hi` are the same, it
just returns a single verse.

:param int lo: Starting day (1 to 12) of the song verse (inclusive).
:param int hi: Ending day (1 to 12) of the song verse (inclusive).

:return list[str]: A list containing the verse for the specified day if both
`lo` and `hi` are the same. Otherwise, it returns a list of strings, each
representing the verse for each day in the specified range.
"""

versify = lambda i=0: I % D[i] + ''.join(G[i:0:-1]) + (i and A or '') + G[0]
"""Generate the verse for the specified day of Christmas.

This function constructs the verse for the given day by combining the
appropriate lines of the song in reverse order, starting from the gifts
received on that day down to the gifts from previous days until the first day.

:param int i: The day of Christmas for which to generate the verse\
(0 to 11, where 0 is the first and 11 is the twelfth day).

:return str: The verse for the specified day of the song.
"""
