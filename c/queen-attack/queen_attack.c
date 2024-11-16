#include "queen_attack.h"
#include <stdlib.h>

attack_status_t can_attack(position_t q1, position_t q2) {
  const uint8_t r1 = q1.row, c1 = q1.column, r2 = q2.row, c2 = q2.column;
  if ((r1 | c1 | r2 | c2) & -8 || (r1 == r2 && c1 == c2)) return 2;
  return (r1 == r2 || c1 == c2 || abs(r2 - r1) == abs(c2 - c1)); }
