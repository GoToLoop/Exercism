MODULE pangram; CONTAINS ! Whether given sentence `s` contains all 26 letters.
  PURE LOGICAL FUNCTION is_pangram(s); CHARACTER(*), INTENT(IN) :: s; mask = 0

    DO i = 1, len_trim(s); lo = ior(ichar(s(i:i)), ichar(' ')) ! lower (bit 5)
      IF (lo < iachar('a') .OR. lo > iachar('z')) CYCLE ! skip non-letter
      mask = ibset(mask, iachar('z') - lo); END DO ! set letter bit on `mask`

    is_pangram = mask == maskr(26); END FUNCTION; END MODULE ! all 26 letters
