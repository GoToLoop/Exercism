// inline static int egg_count(int n) { return n? egg_count(n>>1) + (n&1) : 0; }
inline static int egg_count(int n) { return n? 1 + egg_count(n & (n-1)) : 0; }
