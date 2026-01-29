MODULE perfect_numbers; CONTAINS
  ELEMENTAL CHARACTER*9 FUNCTION classify(n) RESULT(s); INTENT(IN) n
    IF (n <= 0) THEN; s = "ERROR"; RETURN; END IF

    lim = int(sqrt(REAL(n))); ints = -merge(n + lim, n, n == lim*lim)

    DO i = 1, lim; IF (mod(n, i) == 0) ints = ints + i + n/i; END DO

    s = merge("perfect  ", merge("abundant ", "deficient", ints > n), ints == n)
  END FUNCTION classify; END MODULE perfect_numbers
