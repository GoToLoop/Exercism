MODULE reverse_string; CONTAINS
  ELEMENTAL FUNCTION reverse(s) RESULT(rev); CHARACTER(*), INTENT(IN) :: s
    CHARACTER(len(s)) rev; n = len(s)
    FORALL (k=1:n) rev(k:k) = s(n - k + 1:n - k + 1); END FUNCTION; END MODULE
