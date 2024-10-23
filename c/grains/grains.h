// #pragma once

// #define square(n) (n && n < 65 ? 1UL << (n - 1) : 0)
// #define total() ((1UL << 63 << 1) - 1)

inline static long square(const int n) { return n && n < 65? 1L << (n-1) : 0; }
inline static long total() { return (1UL << 63 << 1) - 1; }
