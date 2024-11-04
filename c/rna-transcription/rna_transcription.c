#include "rna_transcription.h"

#include <stdlib.h>
#include <string.h>

#define alloc(str) calloc(strlen(str) + 1, 1)
#define hack(ch) "UCG A"[ch % 5]

/* inline static char *clone(const char *src) { // same as strdup()
  char *c = malloc(strlen(src) + 1); if (c) strcpy(c, src); return c; }
 */

/* static char shift(const char c) {
  int i = -1; while (++i < 4) if ("GCTA"[i] == c) return "CGAU"[i]; return 0; }
 */

char *to_rna(const char *dna) {
  char *r = alloc(dna), *t = r; while (*dna) *t++ = hack(*dna++); return r; }
