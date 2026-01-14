MODULE raindrops; CONTAINS

  PURE CHARACTER(15) FUNCTION convert(n) RESULT(s); INTENT(IN) n

    INTEGER, PARAMETER :: PRIMES(3) = (/3, 5, 7/)
    CHARACTER(5), PARAMETER :: NOISES(3) = (/"Pling", "Plang", "Plong"/)

    s = ""; DO i = 1, size(PRIMES)
      IF (mod(n, PRIMES(i)) == 0) s(1 + len_trim(s):) = NOISES(i); END DO

    IF (s .EQ. '') WRITE (s, '(I0)') n; END FUNCTION; END MODULE
