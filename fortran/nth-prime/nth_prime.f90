MODULE nth_prime; CONTAINS

  PURE INTEGER FUNCTION prime(n) RESULT(p); INTEGER, INTENT(IN) :: n
    p = merge(2, -1, n == 1); IF (n < 2) RETURN; i = 1; p = 1; DO; p = p + 2
      IF (isPrime(p)) THEN; i = i + 1; IF (i == n) EXIT; END IF; END DO; END

  PURE LOGICAL FUNCTION isPrime(n); INTEGER, INTENT(IN) :: n; isPrime = .TRUE.
    IF (n <= 3) THEN; isPrime = n >= 2; RETURN; END IF
    IF (iand(n, 1)*mod(n, 3) == 0) THEN; isPrime = .FALSE.; RETURN; END IF

    DO i = 5, int(sqrt(REAL(n)) + 1), 6; IF (mod(n, i)*mod(n, i + 2) == 0) THEN
        isPrime = .FALSE.; EXIT; END IF; END DO; END FUNCTION; END MODULE
