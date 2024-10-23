#define C BLACK, BROWN, RED, ORANGE, YELLOW, GREEN, BLUE, VIOLET, GREY, WHITE
typedef enum { C } resistor_band_t;
inline static int color_code(const resistor_band_t c[]) {return 10*c[0] + c[1];}
