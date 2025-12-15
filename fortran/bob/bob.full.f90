MODULE bob; CONTAINS

  !----------------------------------------------------------------!
  ! strip(s): remove spaces, tabs, newlines, carriage returns      !
  ! from the input string s and return the cleaned string.         !
  !----------------------------------------------------------------!
  PURE FUNCTION strip(s) RESULT(clean)
    CHARACTER(*), INTENT(IN) :: s; CHARACTER(:), ALLOCATABLE :: clean
    LOGICAL(1) keep(len(s)) ! mask array: .TRUE. if character is kept
    CHARACTER(*), PARAMETER :: SPACES = ' '//achar(9)//achar(10)//achar(13)

    ! Mark which char indices to `keep` (whitespaces filtered out)
    DO i = 1, size(keep); keep(i) = scan(s(i:i), SPACES) == 0; END DO

    ! Allocate output string `clean` to the exact length w/o whitespaces
    ALLOCATE (CHARACTER(count(keep)) :: clean); j = 1

    ! Copy kept characters into the output string `clean`
    DO i = 1, size(keep); IF (keep(i)) THEN
        clean(j:j) = s(i:i); j = j + 1; END IF; END DO; END

  !----------------------------------------------------------------!
  ! hey(s): respond like "Bob" from the classic exercise.          !
  ! Chooses a canned response based on whether the input is empty, !
  ! a question, shouted (all caps), or normal.                     !
  !----------------------------------------------------------------!
  PURE CHARACTER(33) FUNCTION hey(s)
    CHARACTER(33), PARAMETER :: HEYS(5) = (/ &
      & "Fine. Be that way!               ", & ! 1: empty silent input
      & "Calm down, I know what I'm doing!", & ! 2: shouted question
      & "Sure.                            ", & ! 3: normal question
      & "Whoa, chill out!                 ", & ! 4: shouted statement
      & "Whatever.                        "/)  ! 5: normal statement

    CHARACTER(*), INTENT(IN) :: s; CHARACTER(:), ALLOCATABLE :: t
    LOGICAL(1) ask, yell, non_lower, has_upper ! flags

    ! Empty silent input → response 1
    t = strip(s); IF (len(t) .EQ. 0) THEN; hey = HEYS(1); RETURN; END IF

    ! Determine if it's a yell input by making sure no lower letters are found
    non_lower = scan(s, 'abcdefghijklmnopqrstuvwxyz') == 0

    ! Determine if input has at least one uppercase letter for a shout
    has_upper = scan(t, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ') /= 0

    ! Shouting if no lowercase and at least one uppercase
    yell = non_lower .AND. has_upper; ask = t(len(t):) .EQ. '?'
    ! Question if last character is '?'

    ! Choose response based on flags
    IF (ask) THEN; hey = merge(HEYS(2), HEYS(3), yell)
    ELSE; hey = merge(HEYS(4), HEYS(5), yell); END IF; END FUNCTION; END
