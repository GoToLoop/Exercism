ERRS = 'put base must be >= 2', 'all digits must satisfy 0 <= d < input base'

def rebase(ib: int, digits: list[int], ob: int):
    """Converts a sequence of digits in one base, representing a number,
    into a sequence of digits in another base, representing the same number.

    :param int ib: input base of the input `digits`
    :param list[int] digits: the digits of a number in base `ib`
    :param int ob: the target output base to convert input `digits` into

    :raises ValueError: if either bases < 2 or any `digits` >= `ib` or < 0

    :return list[int]: a new `digits` in base `ib` converted to base `ob`
    """

    if min(ib, ob) < 2: raise ValueError((ib < 2 and 'in' or 'out') + ERRS[0])

    if not all(0 <= d < ib for d in digits): raise ValueError(ERRS[1])

    out = [0]*0; dec = sum(d * ib**e for e, d in enumerate(digits[::-1]))

    while dec > 0: out.append(dec % ob); dec //= ob

    return out.reverse() or print(f'{ib, digits, ob, out = }') or out or [0]
