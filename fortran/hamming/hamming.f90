MODULE hamming; CONTAINS

  LOGICAL FUNCTION compute(s1, s2, d)
    CHARACTER(*), INTENT(IN) :: s1, s2; INTEGER, INTENT(OUT) :: d

    compute = len(s1) .EQ. len(s2); d = 0

    IF (compute) d = count([(s1(i:i) .NE. s2(i:i), i=1, len(s1))]); END; END
