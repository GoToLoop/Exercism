MODULE acronym; CONTAINS
  PURE FUNCTION abbreviate(s) RESULT(a); CHARACTER(*), INTENT(IN) :: s
    CHARACTER(:), ALLOCATABLE :: a; a = ''; i = 1; DO

      k = scan(s(i:), 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz')

      IF (k == 0) EXIT; i = i + k - 1; a = a//char(ibclr(ichar(s(i:i)), 5))

      k = scan(s(i:), ' -'); IF (k == 0) EXIT; i = i + k; END DO; END; END
