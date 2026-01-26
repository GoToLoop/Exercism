MODULE high_scores; IMPLICIT NONE; CONTAINS
  PURE FUNCTION personalTopThree(arr) RESULT(top); INTEGER, INTENT(IN) :: arr(:)
    INTEGER top(3), i, p; LOGICAL(1) m(size(arr)); m = .TRUE.; DO i = 1, 3
      p = maxloc(arr, 1, m); top(i) = arr(p); m(p) = .FALSE.; END DO; END

  PURE FUNCTION scores(arr); INTEGER, INTENT(IN) :: arr(:)
    INTEGER scores(size(arr)); scores = arr; END FUNCTION

  PURE INTEGER FUNCTION latest(arr); INTEGER, INTENT(IN) :: arr(:)
    latest = arr(size(arr)); END FUNCTION

  PURE INTEGER FUNCTION personalBest(arr); INTEGER, INTENT(IN) :: arr(:)
    personalBest = maxval(arr); END FUNCTION; END MODULE
