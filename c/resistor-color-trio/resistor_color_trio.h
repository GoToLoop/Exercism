#define C BLACK, BROWN, RED, ORANGE, YELLOW, GREEN, BLUE, VIOLET, GREY, WHITE

typedef enum { C } resistor_band_t; enum { OHMS, KILOOHMS, MEGAOHMS, GIGAOHMS };
typedef struct { unsigned short value; unsigned char unit; } resistor_value_t;

inline static resistor_value_t color_code(const resistor_band_t r[]) {
  const short a = !r[1], b = a + r[2], c = b % 3, d = !c? 1 : c == 1? 10 : 100;
  return (resistor_value_t) { d * ((a? 1 : 10) * r[0] + r[1]), b / 3 }; }
