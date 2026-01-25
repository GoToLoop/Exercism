MODULE triangle; INTERFACE scalene; MODULE PROCEDURE scal, i_scal; END INTERFACE
  INTERFACE equilateral; MODULE PROCEDURE equi, i_equi; END INTERFACE
  INTERFACE isosceles; MODULE PROCEDURE isos, i_isos; END INTERFACE; CONTAINS

  PURE LOGICAL FUNCTION valid(t); REAL, INTENT(IN) :: t(3) ! is triangle
    valid = all(t > 0) .AND. 2*maxval(t) < sum(t); END FUNCTION

  PURE LOGICAL FUNCTION i_valid(t); INTEGER, INTENT(IN) :: t(3) ! is triangle
    i_valid = all(t > 0) .AND. 2*maxval(t) < sum(t); END FUNCTION

  PURE FUNCTION uni(arr); REAL, INTENT(IN) :: arr(:); ALLOCATABLE uni(:)
    uni = pack(arr, [(.NOT. any(arr(i) == arr(:i - 1)), i=1, size(arr))]); END

  PURE FUNCTION i_uni(arr); INTEGER, INTENT(IN) :: arr(:); ALLOCATABLE i_uni(:)
    i_uni = pack(arr, [(.NOT. any(arr(i) == arr(:i - 1)), i=1, size(arr))]); END

  PURE LOGICAL FUNCTION scal(t); REAL, INTENT(IN) :: t(3) ! scalene
    scal = valid(t) .AND. size(uni(t)) == 3; END FUNCTION

  PURE LOGICAL FUNCTION i_scal(t); INTEGER, INTENT(IN) :: t(3) ! scalene
    i_scal = i_valid(t) .AND. size(i_uni(t)) == 3; END FUNCTION

  PURE LOGICAL FUNCTION equi(t); REAL, INTENT(IN) :: t(3) ! equilateral
    equi = valid(t) .AND. size(uni(t)) == 1; END FUNCTION

  PURE LOGICAL FUNCTION i_equi(t); INTEGER, INTENT(IN) :: t(3) ! equilateral
    i_equi = i_valid(t) .AND. size(i_uni(t)) == 1; END FUNCTION

  PURE LOGICAL FUNCTION isos(t); REAL, INTENT(IN) :: t(3) ! isosceles
    l = size(uni(t)); isos = valid(t) .AND. l > 0 .AND. l < 3; END FUNCTION

  PURE LOGICAL FUNCTION i_isos(t); INTEGER, INTENT(IN) :: t(3) ! isosceles
    l = size(i_uni(t)); i_isos = i_valid(t) .AND. l > 0 .AND. l < 3; END; END
