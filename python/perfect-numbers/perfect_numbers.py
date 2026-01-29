from math import sqrt; from typing import Literal

ERR = ValueError('Classification is only possible for positive integers.')

def classify(n: int) -> Literal['perfect', 'abundant', 'deficient']:
    """Classify a positive integer by its aliquot sum (sum of proper divisors, 
       excluding itself), per Nicomachus' scheme: perfect if equals n; abundant 
       if greater; deficient if less.

    :param n: int - a positive integer
    :return: str - the classification of the input integer
    :raises ValueError: if `n` is not positive
    """

    if (n := int(n)) <= 0: raise ERR

    lim = int(sqrt(n)); divs = range(1, lim + 1); total = -n - lim*(n == lim**2)

    total += sum(i + n // i for i in divs if not n % i)

    return 'perfect' if total == n else total > n and 'abundant' or 'deficient'
