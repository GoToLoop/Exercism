MODULE perfect_numbers
  IMPLICIT NONE
CONTAINS

  ELEMENTAL CHARACTER(9) FUNCTION classify(n) RESULT(s)
    ! Classify a positive integer n according to Nicomachus' aliquot sum scheme:
    ! - perfect: sum of proper divisors equals n
    ! - abundant: sum of proper divisors greater than n
    ! - deficient: sum of proper divisors less than n
    INTEGER, INTENT(IN) :: n
    INTEGER :: lim, ints, i

    IF (n <= 0) THEN
      s = "ERROR"
      RETURN
    END IF

    lim = int(sqrt(REAL(n)))
    ! Initialize sum of proper divisors:
    ! subtract n itself, and also subtract lim if n is a perfect square
    ints = -merge(n + lim, n, n == lim*lim)

    DO i = 1, lim
      IF (mod(n, i) == 0) ints = ints + i + n/i
    END DO

    ! Classification based on aliquot sum
    s = merge("perfect  ", merge("abundant ", "deficient", ints > n), ints == n)

  END FUNCTION classify

END MODULE perfect_numbers

PROGRAM test_drive
  USE perfect_numbers
  CHARACTER classifications(30)*9
  INTEGER :: nums(size(classifications)) = (/(i, i=1, size(classifications))/)

  classifications = classify(nums)

  PRINT *, "Nicomachus' aliquot sum classification:"

  DO i = 1, size(classifications)
    PRINT '(I3,2X,A)', i, classifications(i)
  END DO

END PROGRAM test_drive
