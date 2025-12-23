MODULE rna_transcription; CONTAINS

  PURE FUNCTION to_rna(dna); CHARACTER(5), PARAMETER :: HACK = "UCG A"
    CHARACTER(*), INTENT(IN) :: dna; CHARACTER(len_trim(dna)) to_rna

    DO i = 1, len(to_rna); j = 1 + mod(ichar(dna(i:i)), 5)
      to_rna(i:i) = HACK(j:j); END DO; END FUNCTION; END MODULE
