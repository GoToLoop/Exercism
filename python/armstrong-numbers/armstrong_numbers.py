def is_armstrong_number(n: int):
    num_str = str( n := abs(int(n)) ); num_digits = len(num_str)
    return n == sum( int(digit) ** num_digits for digit in num_str )
