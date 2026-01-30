MODULE grains; CONTAINS

  ELEMENTAL DOUBLE PRECISION FUNCTION square(n); INTENT(IN) n; square = -1
    IF (n > 0 .AND. n < 65) square = 2d0**(n - 1); END FUNCTION

  PURE DOUBLE PRECISION FUNCTION total(); total = 2d0**64 - 1; END; END
