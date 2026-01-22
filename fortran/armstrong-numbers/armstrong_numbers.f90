MODULE armstrong_numbers; CONTAINS
  PURE LOGICAL FUNCTION isArmstrongNumber(n) RESULT(a); VALUE n; CHARACTER*10 s

    n = abs(n); WRITE (s, '(I0)') n; l = len_trim(s) ! number of digits `l`

    a = n == sum([((ichar(s(i:i)) - ichar('0'))**l, i=1, l)]); END; END
