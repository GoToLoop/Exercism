to_rna = lambda dna='', RNA=str.maketrans('GCTA', 'CGAU'): dna.translate(RNA)
