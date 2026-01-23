MODULE leap; CONTAINS ! leap = iand(y, merge(15, 3, mod(y, 100) == 0)) == 0

  PURE LOGICAL FUNCTION is_leap_year(y) RESULT(leap); INTEGER, INTENT(IN) :: y

    leap = mod(y, merge(400, 4, mod(y, 100) == 0)) == 0; END FUNCTION; END
