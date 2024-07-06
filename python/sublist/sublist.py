"""
This exercise stub and the test suite contain several enumerated constants.

Enumerated constants can be done with a NAME assigned to an arbitrary,
but unique value. An integer is traditionally used because it’s memory
efficient.
It is a common practice to export both constants and functions that work with
those constants (ex. the constants in the os, subprocess and re modules).

You can learn more here: https://en.Wikipedia.org/wiki/Enumerated_type
"""

# Possible sublist categories. Change the values as you see fit:
UNEQUAL, EQUAL, SUBLIST, SUPERLIST = -1, 0, 1, 2

# j() turns a list of integers or strings into an ASCII string representation:
j, s = lambda i: ''.join(map(s, i)), lambda e: e if type(e) is str else chr(e)

def sublist(one: list, two: list):
    if len(a := j(one)) > len(b := j(two)) and b in a: return SUPERLIST
    return EQUAL if a == b else a in b and SUBLIST or UNEQUAL
