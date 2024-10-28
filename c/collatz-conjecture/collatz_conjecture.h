static const char ERROR_VALUE = -1; inline static int steps(int n) {
  int c = 0; if (n < 1) return ERROR_VALUE;
  while (n != 1) { n = n & 1? 3*n + 1 : n >> 1, ++c; } return c; }
