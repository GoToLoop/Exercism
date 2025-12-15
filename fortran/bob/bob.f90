MODULE bob; CONTAINS

  PURE CHARACTER(33) FUNCTION hey(s); LOGICAL(1) ask, yell, non_lower, has_upper
    CHARACTER(*), INTENT(IN) :: s; CHARACTER(:), ALLOCATABLE :: t

    CHARACTER(33), PARAMETER :: HEYS(5) = (/ &
      & "Fine. Be that way!               ", & ! 1: empty silent input
      & "Calm down, I know what I'm doing!", & ! 2: shouted question
      & "Sure.                            ", & ! 3: normal question
      & "Whoa, chill out!                 ", & ! 4: shouted statement
      & "Whatever.                        "/)  ! 5: normal statement

    t = strip(s); IF (len(t) .EQ. 0) THEN; hey = HEYS(1); RETURN; END IF

    non_lower = scan(t, 'abcdefghijklmnopqrstuvwxyz') == 0 ! no lower chars
    has_upper = scan(t, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ') /= 0 ! at least 1 upper

    yell = non_lower .AND. has_upper; ask = t(len(t):) .EQ. '?'

    IF (ask) THEN; hey = merge(HEYS(2), HEYS(3), yell) ! choose based on flags
    ELSE; hey = merge(HEYS(4), HEYS(5), yell); END IF; END FUNCTION

  PURE FUNCTION strip(s) RESULT(clean); CHARACTER ch
    CHARACTER(*), INTENT(IN) :: s; CHARACTER(:), ALLOCATABLE :: clean
    CHARACTER(*), PARAMETER :: SPACES = ' '//achar(9)//achar(10)//achar(13)

    j = 0; num = len_trim(s); ALLOCATE (CHARACTER(num) :: clean)

    DO i = 1, num; ch = s(i:i); IF (scan(ch, SPACES) .EQ. 0) THEN
        j = j + 1; clean(j:j) = ch; END IF; END DO; clean = clean(:j); END; END
