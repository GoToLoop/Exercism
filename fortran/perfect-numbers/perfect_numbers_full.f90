MODULE perfect_numbers
  IMPLICIT NONE
CONTAINS

  ELEMENTAL CHARACTER(9) FUNCTION classify(n) RESULT(s)
    ! Classify an integer `n` according to Nicomachus' aliquot sum scheme:
    ! - perfect: sum of proper divisors equals `n`
    ! - abundant: sum of proper divisors greater than `n`
    ! - deficient: sum of proper divisors less than `n`

    INTEGER, INTENT(IN) :: n  ! input number to classify

    INTEGER &
      lim, &                  ! integer square root of `n`
      ints, &                 ! running total of proper divisors
      i                       ! loop index for divisor search

    IF (n <= 0) THEN
      s = "ERROR"             ! invalid input classification
      RETURN
    END IF

    lim = int(sqrt(REAL(n)))  ! compute integer square root of `n`

    ! Initialize sum of proper divisors:
    ! subtract `n` itself, and also subtract `lim` if `n` is a perfect square
    ints = -merge(n + lim, n, n == lim*lim)

    DO i = 1, lim             ! loop through possible divisors up to `sqrt(n)`
      ! Add both divisor `i` and its paired divisor `n/i`:
      IF (mod(n, i) == 0) ints = ints + i + n/i
    END DO

    ! Classification based on Nicomachus' aliquot sum:
    s = merge("perfect  ", merge("abundant ", "deficient", ints > n), ints == n)

  END FUNCTION classify

END MODULE perfect_numbers

PROGRAM test_drive
  USE perfect_numbers

  ! Array of classification strings
  CHARACTER classifications(30)*9

  ! Array of integers 1..30
  INTEGER :: nums(size(classifications)) = (/(i, i=1, size(classifications))/)

  ! Apply classify() element-wise to the whole array
  classifications = classify(nums)

  PRINT *, "Nicomachus' aliquot sum classification:"

  ! Print each number with its classification
  DO i = 1, size(classifications)
    PRINT '(I3,2X,A)', i, classifications(i)
  END DO

END PROGRAM test_drive
