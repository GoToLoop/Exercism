def steps(n: int):
    if type(n) is not int: raise TypeError("Only int value types are allowed")
    if n < 1: raise ValueError("Only positive integers are allowed")

    steps = 0
    while n != 1: n = n & 1 and 3*n + 1 or n >> 1; steps += 1
    return steps
