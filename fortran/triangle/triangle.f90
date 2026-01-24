MODULE triangle; INTERFACE scalene; MODULE PROCEDURE scal, scal_i; END INTERFACE
  INTERFACE equilateral; MODULE PROCEDURE equi, equi_i; END INTERFACE
  INTERFACE isosceles; MODULE PROCEDURE isos, isos_i; END INTERFACE; CONTAINS

  PURE LOGICAL FUNCTION valid(t); REAL, INTENT(IN) :: t(3) ! is triangle
    valid = all(t > 0) .AND. 2*maxval(t) < sum(t); END FUNCTION

  PURE LOGICAL FUNCTION valid_i(t); INTEGER, INTENT(IN) :: t(3) ! is triangle
    valid_i = all(t > 0) .AND. 2*maxval(t) < sum(t); END FUNCTION

  PURE FUNCTION unique(arr) RESULT(uni); REAL, INTENT(IN) :: arr(:)
    REAL, ALLOCATABLE :: uni(:); uni = [arr(1)]; DO i = 2, size(arr)

      a = arr(i); IF (.NOT. any(uni == a)) uni = [uni, a]; END DO; END FUNCTION

  PURE FUNCTION unique_i(arr) RESULT(uni); INTEGER, INTENT(IN) :: arr(:)
    INTEGER, ALLOCATABLE :: uni(:); uni = [arr(1)]; DO i = 2, size(arr)

      n = arr(i); IF (.NOT. any(uni == n)) uni = [uni, n]; END DO; END FUNCTION

  PURE LOGICAL FUNCTION scal(t); REAL, INTENT(IN) :: t(3) ! scalene
    scal = valid(t) .AND. size(unique(t)) == 3; END FUNCTION

  PURE LOGICAL FUNCTION scal_i(t); INTEGER, INTENT(IN) :: t(3) ! scalene
    scal_i = valid_i(t) .AND. size(unique_i(t)) == 3; END FUNCTION

  PURE LOGICAL FUNCTION equi(t); REAL, INTENT(IN) :: t(3) ! equilateral
    equi = valid(t) .AND. size(unique(t)) == 1; END FUNCTION

  PURE LOGICAL FUNCTION equi_i(t); INTEGER, INTENT(IN) :: t(3) ! equilateral
    equi_i = valid_i(t) .AND. size(unique_i(t)) == 1; END FUNCTION

  PURE LOGICAL FUNCTION isos(t); REAL, INTENT(IN) :: t(3) ! isosceles
    l = size(unique(t)); isos = valid(t) .AND. l > 0 .AND. l < 3; END FUNCTION

  PURE LOGICAL FUNCTION isos_i(t); INTEGER, INTENT(IN) :: t(3) ! isosceles
    l = size(unique_i(t)); isos_i = valid_i(t) .AND. l > 0 .AND. l < 3; END; END
