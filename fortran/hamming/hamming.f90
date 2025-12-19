MODULE hamming; CONTAINS

  LOGICAL FUNCTION compute(s1, s2, d)
    CHARACTER(*), INTENT(IN) :: s1, s2; INTEGER, INTENT(OUT) :: d

    d = 0; compute = len(s1) .EQ. len(s2); IF (compute) THEN
      d = count([(s1(i:i) .NE. s2(i:i), i=1, len(s1))]); END IF; END; END
