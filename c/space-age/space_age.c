#include "space_age.h"

static const uint32_t SEC = 24*60*60*365.25; static const float PERIODS[] = {
.2408467, .61519726, 1, 1.8808158, 11.862615, 29.447498, 84.016846, 164.79132 };

inline float age(const planet_t p, const int64_t sec) {
  return p >= MERCURY && p <= NEPTUNE ? sec / (SEC * PERIODS[p]) : -1; }
