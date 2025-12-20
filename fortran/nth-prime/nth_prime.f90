MODULE nth_prime; CONTAINS

  PURE INTEGER FUNCTION prime(n); INTEGER, INTENT(IN) :: n; prime = -1
    IF (n < 1) RETURN; i = 0; prime = 1; DO; prime = prime + 1
      IF (isPrime(prime)) THEN; i = i + 1; IF (i == n) EXIT; END IF; END DO; END

  PURE LOGICAL FUNCTION isPrime(n); INTEGER, INTENT(IN) :: n; isPrime = .TRUE.
    IF (n <= 3) THEN; isPrime = n >= 2; RETURN; END IF
    IF (iand(n, 1)*mod(n, 3) == 0) THEN; isPrime = .FALSE.; RETURN; END IF

    DO i = 5, int(sqrt(REAL(n)) + 1), 6; IF (mod(n, i)*mod(n, i + 2) == 0) THEN
        isPrime = .FALSE.; EXIT; END IF; END DO; END FUNCTION; END MODULE
