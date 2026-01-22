MODULE collatz_conjecture; CONTAINS
  PURE INTEGER FUNCTION steps(n) RESULT(c); VALUE n

    c = merge(0, -1, n > 0); DO WHILE (n > 1)
      c = c + 1; n = merge(n/2, 3*n + 1, iand(n, 1) == 0); END DO; END; END
