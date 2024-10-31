#include "rna_transcription.h"

#include <stdlib.h>
#include <string.h>

static const char *const DNA = "GCTA", *const RNA = "CGAU";

inline static char shift(char a) {
  int i = -1; while (++i < 4) if (DNA[i] == a) return RNA[i]; return 0; }

inline static char *clone(const char *s) { // strdup()
  long l = 1+strlen(s); char *c = malloc(l); if (c) memcpy(c, s, l); return c; }

char *to_rna(const char *dna) {
  char *r = clone(dna), *t = r; while (*dna) *t++ = shift(*dna++); return r; }
