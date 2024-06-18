def is_valid(isbn: str, REV=slice(-2, None, -1)):
    if not pre_check(isbn := isbn.replace('-', '')): return False
    gen = (int(n) * i for i, n in enumerate(isbn[REV], 2))
    return not (sum(gen) + (isbn[-1] == 'X' and 10 or int(isbn[-1]))) % 11

pre_check = lambda digits='': len(digits) == 10 and digits[:-1].isdigit() and (
    digits[-1].isdigit() or digits[-1] == 'X')