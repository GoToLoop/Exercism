#define sum_of_squares(n) (sums(n) * (2*n + 1) / 3)
#define difference_of_squares(n) (square_of_sum(n) - sum_of_squares(n))

inline static int sums(const int n) { return n * (n + 1) >> 1; }
inline static int square_of_sum(int n) { n = sums(n); return n * n; }
