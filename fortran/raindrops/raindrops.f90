MODULE raindrops; CONTAINS
  PURE CHARACTER*15 FUNCTION convert(n) RESULT(s); INTENT(IN) n

    INTEGER(1), PARAMETER :: PRIMES(3) = (/3_1, 5_1, 7_1/)
    CHARACTER, PARAMETER :: NOISES(3)*5 = (/"Pling", "Plang", "Plong"/)

    s = ""; DO i = 1, size(PRIMES)
      IF (mod(n, int(PRIMES(i))) == 0) s(1 + len_trim(s):) = NOISES(i); END DO

    IF (s .EQ. '') WRITE (s, '(I0)') n; END FUNCTION; END MODULE
