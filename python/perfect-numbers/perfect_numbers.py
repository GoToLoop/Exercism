from math import sqrt

ERR = 'Classification is only possible for positive integers.'

def classify(n: int):
    """A perfect number equals the sum of its positive divisors.

    :param n: int a positive integer
    :return: str the classification of the input integer
    """

    if (n := int(n)) <= 0: raise ValueError(ERR)

    total, div, divs = -n, 0, range(1, int(sqrt(n)) + 1)

    total += sum(i + (div := n // i) * (div != i) for i in divs if not n % i)

    return 'perfect' if total == n else total > n and 'abundant' or 'deficient'
