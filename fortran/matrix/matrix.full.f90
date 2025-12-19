MODULE matrix; CONTAINS ! Get an int row or column from a str matrix of numbers

  PURE FUNCTION row(m, d, i) RESULT(r); INTEGER, INTENT(IN) :: d(2), i
    CHARACTER(*), INTENT(IN) :: m(d(1)); INTEGER r(d(2)); r = isplit(m(i)); END

  PURE FUNCTION column(m, d, j) RESULT(c); INTEGER, INTENT(IN) :: d(2), j
    CHARACTER(*), INTENT(IN) :: m(d(1)); INTEGER c(d(1)), mat(d(1), d(2))
    DO i = 1, d(1); mat(i, :) = isplit(m(i)); END DO; c = mat(:, j); END

  PURE FUNCTION isplit(str, delim) RESULT(nums) ! Split a str as an int array
    CHARACTER(*), INTENT(IN) :: str; CHARACTER, INTENT(IN), OPTIONAL :: delim
    INTEGER, ALLOCATABLE :: nums(:), inds(:); inds = delim_indices(str, delim)

    m = len_trim(str); n = size(inds); ALLOCATE (nums(n + 1))

    IF (n /= 0) THEN; READ (str(:inds(1) - 1), *) nums(1) ! First number

      DO i = 2, n; READ (str(inds(i - 1) + 1:inds(i) - 1), *) nums(i); END DO

      READ (str(inds(n) + 1:m), *) nums(n + 1) ! Last number

    ELSE; READ (str(:m), *) nums(1); END IF; END FUNCTION ! No delim = 1 number

  PURE FUNCTION delim_indices(str, delim) RESULT(inds) ! Get index of each delim
    CHARACTER(*), INTENT(IN) :: str; CHARACTER, INTENT(IN), OPTIONAL :: delim
    INTEGER, ALLOCATABLE :: inds(:); CHARACTER sep

    sep = merge(delim, ' ', present(delim)); n = len_trim(str)
    inds = pack([(i, i=1, n)], [(str(i:i) .EQ. sep, i=1, n)]); END FUNCTION; END
