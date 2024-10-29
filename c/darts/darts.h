typedef struct { float x, y; } coordinate_t;

static const unsigned char SCORES[][2] = { { 1, 10 }, { 25, 5 }, { 100, 1 } };

inline static unsigned char score(const coordinate_t xy) {
  const float d = xy.x * xy.x + xy.y * xy.y;
  for (int i = 0; i < 3; ++i) if (d <= SCORES[i][0]) return SCORES[i][1];
  return 0; }
