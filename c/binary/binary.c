#include "binary.h"

int convert(const char *bin) { unsigned c, d = 0;
  while (*bin) if ((c = *bin++ - '0') < 2) d = d << 1 | c; else return INVALID;
  return d; }
