MODULE reverse_string; CONTAINS
  ELEMENTAL FUNCTION reverse(s) RESULT(rev); CHARACTER(*), INTENT(IN) :: s

    CHARACTER rev*(len(s)); n = len(s); m = shiftr(n, 1); DO CONCURRENT(k=1:m)
      i = n - k + 1; rev(k:k) = s(i:i); rev(i:i) = s(k:k); END DO

    IF (iand(n, 1) == 1) THEN; m = m + 1; rev(m:m) = s(m:m); END IF; END; END
