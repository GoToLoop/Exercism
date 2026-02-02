MODULE reverse_string; CONTAINS
  ELEMENTAL FUNCTION reverse(s) RESULT(rev); CHARACTER(*), INTENT(IN) :: s
    CHARACTER(len(s)) rev; WRITE (rev, '(*(A))') [(s(i:i), i=len(s), 1, -1)]
  END FUNCTION reverse; END MODULE reverse_string
