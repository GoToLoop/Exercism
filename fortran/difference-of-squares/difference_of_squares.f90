MODULE difference_of_squares; CONTAINS

  PURE INTEGER FUNCTION square_of_sum(n); INTEGER, INTENT(IN), VALUE :: n
    square_of_sum = shiftr(n*(n + 1), 1)**2; END

  PURE INTEGER FUNCTION sum_of_squares(n); INTEGER, INTENT(IN), VALUE :: n
    sum_of_squares = n*(n + 1)*(shiftl(n, 1) + 1)/6; END

  PURE INTEGER FUNCTION difference(n); INTEGER, INTENT(IN), VALUE :: n
    difference = square_of_sum(n) - sum_of_squares(n); END; END MODULE
