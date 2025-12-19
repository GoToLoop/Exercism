MODULE matrix; CONTAINS ! Get an int row or column from a str matrix of numbers

  PURE FUNCTION row(m, d, i) RESULT(r); INTEGER, INTENT(IN) :: d(2), i
    CHARACTER(*), INTENT(IN) :: m(d(1)); INTEGER r(d(2)); READ (m(i), *) r; END

  PURE FUNCTION column(m, d, j) RESULT(c); INTEGER, INTENT(IN) :: d(2), j
    CHARACTER(*), INTENT(IN) :: m(d(1)); INTEGER c(d(1)), mat(d(1), d(2))
    DO i = 1, d(1); mat(i, :) = row(m, d, i); END DO; c = mat(:, j); END; END
