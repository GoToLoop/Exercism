from functools import cache

@cache
def square(number: int):
    if type(number) is not int: raise TypeError("only integers are allowed")
    if not 0 < number < 65: raise ValueError("square must be between 1 and 64")

    return 1 << number - 1


def total(): return sum( square(i) for i in range(1, 65) ) # (1 << 64) - 1
